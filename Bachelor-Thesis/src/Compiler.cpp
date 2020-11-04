#include "Compiler.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif

namespace Lang {

	std::shared_ptr<Parser> Compiler::m_Parser;
	std::shared_ptr<Chunk> Compiler::m_Chunk;

	ParseRule Compiler::m_ParseRules[] = {
		{ Grouping,		NULL,	Precedence::None },			// LeftParen
		{ NULL,			NULL,	Precedence::None },			// RightParen
		{ Vector,		NULL,	Precedence::None },			// LeftSquare
		{ NULL,			NULL,	Precedence::None },			// RightSquare
		{ NULL,			NULL,	Precedence::None },			// Comma
		{ NULL,			Binary,	Precedence::Term },			// Plus
		{ Unary,		Binary,	Precedence::Term },			// Minus
		{ NULL,			Binary,	Precedence::Factor },		// Star
		{ NULL,			Binary,	Precedence::Factor },		// Slash
		{ NULL,			NULL,	Precedence::None },			// Equal
		{ NULL,			Binary,	Precedence::Equality },		// EqualEqual
		{ Unary,		NULL,	Precedence::None },			// Bang
		{ NULL,			Binary,	Precedence::Equality },		// BangEqual
		{ NULL,			Binary,	Precedence::Comparison },	// Greater
		{ NULL,			Binary,	Precedence::Comparison },	// GreaterEqual
		{ NULL,			Binary,	Precedence::Comparison },	// Less
		{ NULL,			Binary,	Precedence::Comparison },	// LessEqual
		{ NULL,			Binary,	Precedence::Comparison },	// And
		{ NULL,			Binary,	Precedence::Comparison },	// Or
		{ Number,		NULL,	Precedence::None },			// Number
		{ Variable,		NULL,	Precedence::None },			// Identifier
		{ NULL,			NULL,	Precedence::None },			// Function
		{ DimExpr,		NULL,	Precedence::None },			// Dim
		{ ShapeExpr,	NULL,	Precedence::None },			// Shape
		{ SelExpr,		NULL,	Precedence::None },			// Sel
		{ LetExpr,		NULL,	Precedence::None },			// Let
		{ NULL,			NULL,	Precedence::None },			// In
		{ IfExpr,		NULL,	Precedence::None },			// If
		{ NULL,			NULL,	Precedence::None },			// Then
		{ NULL,			NULL,	Precedence::None },			// Else
		{ NULL,			NULL,	Precedence::None },			// Error
		{ NULL,			NULL,	Precedence::None },			// Eof
	};

	bool Compiler::Compile(const char* source, std::shared_ptr<Chunk> chunk) {
		m_Parser = std::make_shared<Parser>(source);
		if (!m_Parser->Parse()) return false;
		m_Parser->Print();

		m_Chunk = chunk;
		Expression();
		EndCompiler();

		return true;
	}

	void Compiler::EndCompiler() {
		EmitByte((uint8_t)OpCode::Return);
		#ifdef DEBUG
		Disassembler::Disassemble(m_Chunk, "Code");
		#endif
	}

	void Compiler::Grouping(bool canAssign) {
		Expression();
		m_Parser->Consume(TokenType::RightParen, "Expect `)' after expression");
	}

	void Compiler::Expression() {
		m_Parser->Advance();
		ParsePrecedence(Precedence::Assignment);
		m_Parser->Synchronize();
	}

	void Compiler::DimExpr(bool canAssign) {
		m_Parser->Consume(TokenType::LeftParen, "Expect `(' after dim expression");
		Expression();

		m_Parser->Consume(TokenType::RightParen, "Expect `)' after dim arguments");
		EmitByte((uint8_t)OpCode::Dim);
	}

	void Compiler::ShapeExpr(bool canAssign) {
		m_Parser->Consume(TokenType::LeftParen, "Expect `(' after shape expression");
		Expression();

		m_Parser->Consume(TokenType::RightParen, "Expect `)' after shape arguments");
		EmitByte((uint8_t)OpCode::Shape);
	}

	void Compiler::SelExpr(bool canAssign) {
		m_Parser->Consume(TokenType::LeftParen, "Expect `(' after sel expression");
		Expression();

		m_Parser->Consume(TokenType::Comma, "Expect `,' after first sel argument");
		Expression();

		m_Parser->Consume(TokenType::RightParen, "Expect `)' after sel arguments");
		EmitByte((uint8_t)OpCode::Sel);
	}

	void Compiler::LetExpr(bool canAssign) {
		Token token = m_Parser->Advance();
		std::string name(token.Start, token.Length);
		uint8_t index = m_Chunk->AddVariable(name);

		m_Parser->Consume(TokenType::Equal, "Expect `=' in let expression");

		Expression();
		EmitBytes((uint8_t)OpCode::SetVariable, index);

		m_Parser->Consume(TokenType::In, "Expect `in' after let expression");
		Expression();
		EmitByte((uint8_t)OpCode::PopVariable);
	}

	void Compiler::IfExpr(bool canAssign) {
		Expression();

		m_Parser->Consume(TokenType::Then, "Expect `then' in if expression");
		int thenJump = EmitJump(OpCode::JumpIfFalse);
		EmitByte((uint8_t)OpCode::Pop);
		Expression();

		m_Parser->Consume(TokenType::Else, "Expect `else' in if expression");
		int elseJump = EmitJump(OpCode::Jump);
		PatchJump(thenJump);
		EmitByte((uint8_t)OpCode::Pop);

		Expression();
		PatchJump(elseJump);
	}

	void Compiler::Variable(bool canAssign) {
		Token token = m_Parser->Peek();
		std::string name(token.Start, token.Length);
		uint8_t index = m_Chunk->GetVariable(name);
		EmitBytes((uint8_t)OpCode::GetVariable, index);
	}

	void Compiler::Vector(bool canAssign) {
		uint8_t dim = 1;
		std::vector<uint16_t> shape = { 0 };
		std::vector<double> values;

		while (m_Parser->Match(TokenType::Number)) {
			shape[0]++;
			values.push_back(strtod(m_Parser->Peek().Start, NULL));
			if (!m_Parser->Match(TokenType::Comma)) {
				break;
			}
		}

		m_Parser->Consume(TokenType::RightSquare, "Expect `]' after vector");
		EmitConstant({ dim, shape, values });
	}

	void Compiler::Number(bool canAssign) {
		double value = strtod(m_Parser->Peek().Start, NULL);
		EmitConstant(value);
	}

	void Compiler::Binary(bool canAssign) {
		TokenType operatorType = m_Parser->Peek().Type;
		ParseRule* rule = GetRule(operatorType);
		ParsePrecedence((Precedence)((int)rule->Precedence + 1));

		switch (operatorType) {
		case TokenType::EqualEqual:		EmitByte((uint8_t)OpCode::Equal); break;
		case TokenType::BangEqual:		EmitByte((uint8_t)OpCode::NotEqual); break;
		case TokenType::Greater:		EmitByte((uint8_t)OpCode::Greater); break;
		case TokenType::GreaterEqual:	EmitByte((uint8_t)OpCode::GreaterEqual); break;
		case TokenType::Less:			EmitByte((uint8_t)OpCode::Less); break;
		case TokenType::LessEqual:		EmitByte((uint8_t)OpCode::LessEqual); break;
		case TokenType::And:			EmitByte((uint8_t)OpCode::And); break;
		case TokenType::Or:				EmitByte((uint8_t)OpCode::Or); break;
		case TokenType::Plus:			EmitByte((uint8_t)OpCode::Add); break;
		case TokenType::Minus:			EmitByte((uint8_t)OpCode::Subtract); break;
		case TokenType::Star:			EmitByte((uint8_t)OpCode::Multiply); break;
		case TokenType::Slash:			EmitByte((uint8_t)OpCode::Divide); break;

		default:
			fprintf(stderr, "Invalid operator `%d'", operatorType);
			return;
		}
	}

	void Compiler::Unary(bool canAssign) {
		TokenType operatorType = m_Parser->Peek().Type;
		Expression();
		ParsePrecedence(Precedence::Unary);

		switch (operatorType) {
		case TokenType::Bang:	EmitByte((uint8_t)OpCode::Not); break;
		case TokenType::Minus:	EmitByte((uint8_t)OpCode::Negate); break;

		default:
			fprintf(stderr, "Invalid operator `%d'", operatorType);
			return;
		}
	}

	void Compiler::ParsePrecedence(Precedence precedence) {
		ParseFn prefixRule = GetRule(m_Parser->Peek().Type)->Prefix;
		if (prefixRule == NULL) {
			m_Parser->Error(&m_Parser->Peek(), "Expect expression");
			return;
		}

		bool canAssign = precedence <= Precedence::Assignment;
		prefixRule(canAssign);

		while (precedence <= GetRule(m_Parser->PeekNext().Type)->Precedence) {
			m_Parser->Advance();
			ParseFn infixRule = GetRule(m_Parser->Peek().Type)->Infix;
			infixRule(canAssign);
		}

		if (canAssign && m_Parser->Match(TokenType::Equal)) {
			m_Parser->Error(&m_Parser->Peek(), "Invalid assignment target");
		}
	}

	void Compiler::EmitByte(uint8_t byte) {
		m_Chunk->Write(byte, m_Parser->Peek().Line);
	}

	void Compiler::EmitBytes(uint8_t byte1, uint8_t byte2) {
		EmitByte(byte1);
		EmitByte(byte2);
	}

	void Compiler::EmitConstant(Value value) {
		EmitBytes((uint8_t)OpCode::Constant, MakeConstant(value));
	}

	uint8_t Compiler::MakeConstant(Value value) {
		return m_Chunk->AddConstant(value);
	}

	int Compiler::EmitJump(OpCode opCode) {
		EmitByte((uint8_t)opCode);
		EmitByte(0xff);
		EmitByte(0xff);
		return (int)m_Chunk->Code.size() - 2;
	}

	void Compiler::PatchJump(int offset) {
		int jump = (int)m_Chunk->Code.size() - offset - 2;
		if (jump > UINT16_MAX) {
			m_Parser->Error(&m_Parser->Peek(), "Expression too large to jump over");
		}

		m_Chunk->Code[offset] = (jump >> 8) & 0xff;
		m_Chunk->Code[offset + 1] = jump & 0xff;
	}

	ParseRule* Compiler::GetRule(TokenType type) {
		return &m_ParseRules[(uint8_t)type];
	}

}

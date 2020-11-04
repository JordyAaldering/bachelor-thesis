#include "Compiler.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif

namespace Lang {

	Parser Compiler::m_Parser;
	std::shared_ptr<Chunk> Compiler::m_Chunk;
	bool Compiler::m_InPanicMode;
	int Compiler::m_ParserIndex;

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
		m_Parser = Parser();
		m_Parser.Parse(source);
		if (m_Parser.HadError()) return false;
		m_Parser.Print();

		m_Chunk = chunk;
		m_InPanicMode = false;
		m_ParserIndex = -1;

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
		Consume(TokenType::RightParen, "Expect `)' after expression");
	}

	void Compiler::Expression() {
		Advance();
		ParsePrecedence(Precedence::Assignment);
		if (m_InPanicMode) {
			Synchronize();
		}
	}

	void Compiler::DimExpr(bool canAssign) {
		Consume(TokenType::LeftParen, "Expect `(' after dim expression");
		Expression();

		Consume(TokenType::RightParen, "Expect `)' after dim arguments");
		EmitByte((uint8_t)OpCode::Dim);
	}

	void Compiler::ShapeExpr(bool canAssign) {
		Consume(TokenType::LeftParen, "Expect `(' after shape expression");
		Expression();

		Consume(TokenType::RightParen, "Expect `)' after shape arguments");
		EmitByte((uint8_t)OpCode::Shape);
	}

	void Compiler::SelExpr(bool canAssign) {
		Consume(TokenType::LeftParen, "Expect `(' after sel expression");
		Expression();

		Consume(TokenType::Comma, "Expect `,' after first sel argument");
		Expression();

		Consume(TokenType::RightParen, "Expect `)' after sel arguments");
		EmitByte((uint8_t)OpCode::Sel);
	}

	void Compiler::LetExpr(bool canAssign) {
		Advance();
		Token token = m_Parser.Get(m_ParserIndex);
		std::string name(token.Start, token.Length);
		uint8_t index = m_Chunk->AddVariable(name);
		Consume(TokenType::Equal, "Expect `=' in let expression");

		Expression();
		EmitBytes((uint8_t)OpCode::SetVariable, index);

		Consume(TokenType::In, "Expect `in' after let expression");
		Expression();
		EmitByte((uint8_t)OpCode::PopVariable);
	}

	void Compiler::IfExpr(bool canAssign) {
		Expression();

		Consume(TokenType::Then, "Expect `then' in if expression");
		int thenJump = EmitJump(OpCode::JumpIfFalse);
		EmitByte((uint8_t)OpCode::Pop);
		Expression();

		Consume(TokenType::Else, "Expect `else' in if expression");
		int elseJump = EmitJump(OpCode::Jump);
		PatchJump(thenJump);
		EmitByte((uint8_t)OpCode::Pop);

		Expression();
		PatchJump(elseJump);
	}

	void Compiler::Variable(bool canAssign) {
		Token token = m_Parser.Get(m_ParserIndex);
		std::string name(token.Start, token.Length);
		uint8_t index = m_Chunk->GetVariable(name);
		EmitBytes((uint8_t)OpCode::GetVariable, index);
	}

	void Compiler::Vector(bool canAssign) {
		uint8_t dim = 1;
		std::vector<uint16_t> shape = { 0 };
		std::vector<double> values;

		while (Match(TokenType::Number)) {
			shape[0]++;
			values.push_back(strtod(m_Parser.Get(m_ParserIndex).Start, NULL));
			if (!Match(TokenType::Comma)) {
				break;
			}
		}

		Consume(TokenType::RightSquare, "Expect `]' after vector");
		EmitConstant({ dim, shape, values });
	}

	void Compiler::Number(bool canAssign) {
		double value = strtod(m_Parser.Get(m_ParserIndex).Start, NULL);
		EmitConstant(value);
	}

	void Compiler::Binary(bool canAssign) {
		TokenType operatorType = m_Parser.Get(m_ParserIndex).Type;
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
		TokenType operatorType = m_Parser.Get(m_ParserIndex).Type;
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
		//Advance();

		Token token = m_Parser.Get(m_ParserIndex);
		ParseFn prefixRule = GetRule(token.Type)->Prefix;
		if (prefixRule == NULL) {
			Error(&token, "Expect expression");
			return;
		}

		bool canAssign = precedence <= Precedence::Assignment;
		prefixRule(canAssign);

		while (precedence <= GetRule(m_Parser.Get(m_ParserIndex + 1).Type)->Precedence) {
			Advance();
			ParseFn infixRule = GetRule(m_Parser.Get(m_ParserIndex).Type)->Infix;
			infixRule(canAssign);
		}

		if (canAssign && Match(TokenType::Equal)) {
			Error(&m_Parser.Get(m_ParserIndex), "Invalid assignment target");
		}
	}

	void Compiler::Advance() {
		m_ParserIndex++;
	}

	bool Compiler::Check(TokenType type) {
		return m_Parser.Get(m_ParserIndex + 1).Type == type;
	}

	bool Compiler::Match(TokenType type) {
		if (!Check(type)) return false;
		Advance();
		return true;
	}

	void Compiler::Consume(TokenType type, const char* msg) {
		if (Match(type)) return;
		Error(&m_Parser.Get(m_ParserIndex + 1), msg);
	}

	void Compiler::EmitByte(uint8_t byte) {
		m_Chunk->Write(byte, m_Parser.Get(m_ParserIndex).Line);
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
		return m_Chunk->Code.size() - 2;
	}

	void Compiler::PatchJump(int offset) {
		int jump = m_Chunk->Code.size() - offset - 2;
		if (jump > UINT16_MAX) {
			Error(&m_Parser.Get(m_ParserIndex), "Expression is too large to jump over.");
		}

		m_Chunk->Code[offset] = (jump >> 8) & 0xff;
		m_Chunk->Code[offset + 1] = jump & 0xff;
	}

	ParseRule* Compiler::GetRule(TokenType type) {
		return &m_ParseRules[(uint8_t)type];
	}

	void Compiler::Error(Token* token, const char* msg) {
		if (m_InPanicMode) return;
		m_InPanicMode = true;

		fprintf(stderr, "[line %d] Error", token->Line);
		if (token->Type == TokenType::Eof) {
			fprintf(stderr, " at end: %s\n", msg);
		}
		else if (token->Type != TokenType::Error) {
			fprintf(stderr, " at `%.*s': %s\n", token->Length, token->Start, msg);
		}
	}

	void Compiler::Synchronize() {
		m_InPanicMode = false;
		while (Check(TokenType::Eof)) {
			switch (m_Parser.Get(m_ParserIndex + 1).Type) {
				case TokenType::Function:
				case TokenType::Dim:
				case TokenType::Shape:
				case TokenType::Sel:
					return;
			}
		}
	}

}

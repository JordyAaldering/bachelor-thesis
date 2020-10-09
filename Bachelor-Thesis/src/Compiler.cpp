#include "Compiler.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif


namespace Lang {

	typedef void (*ParseFn)();
	typedef struct ParseRule {
		ParseFn Prefix;
		ParseFn Infix;
		Precedence Precedence;
	};

	static ParseRule rules[] = {
		{ Grouping, NULL, Precedence::None }, // LeftParen
		{ NULL, NULL, Precedence::None }, // RightParen
		{ NULL, NULL, Precedence::None }, // LeftBrace
		{ NULL, NULL, Precedence::None }, // RightBrace
		{ NULL, NULL, Precedence::None }, // Dot
		{ NULL, NULL, Precedence::None }, // Comma
		{ NULL, NULL, Precedence::None }, // Semicolon
		{ NULL, Binary, Precedence::None }, // Plus
		{ Unary, Binary, Precedence::None }, // Minus
		{ NULL, Binary, Precedence::Factor }, // Star
		{ NULL, Binary, Precedence::Factor }, // Slash
		{ NULL, NULL, Precedence::None }, // Bang
		{ NULL, NULL, Precedence::None }, // BangEqual
		{ NULL, NULL, Precedence::None }, // Equal
		{ NULL, NULL, Precedence::None }, // EqualEqual
		{ NULL, NULL, Precedence::None }, // Greater
		{ NULL, NULL, Precedence::None }, // GreaterEqual
		{ NULL, NULL, Precedence::None }, // Less
		{ NULL, NULL, Precedence::None }, // LessEqual
		{ NULL, NULL, Precedence::None }, // And
		{ NULL, NULL, Precedence::None }, // Or
		{ Number, NULL, Precedence::None }, // Number
		{ NULL, NULL, Precedence::None }, // Identifier
		{ NULL, NULL, Precedence::None }, // Var
		{ NULL, NULL, Precedence::None }, // Fun
		{ NULL, NULL, Precedence::None }, // If
		{ NULL, NULL, Precedence::None }, // Else
		{ NULL, NULL, Precedence::None }, // Dim
		{ NULL, NULL, Precedence::None }, // Shape
		{ NULL, NULL, Precedence::None }, // Sel
		{ NULL, NULL, Precedence::None }, // Error
		{ NULL, NULL, Precedence::None }, // Eof
	};

	static ParseRule* GetRule(TokenType type) {
		return &rules[type];
	}

	Compiler::Compiler(const char* source)
		: m_Scanner(source) {
	}

	bool Compiler::Compile(std::shared_ptr<Chunk> chunk) {
		m_CompilingChunk = chunk;
		m_Parser.HadError = false;
		m_Parser.InPanicMode = false;

		Advance();
		Expression();
		Consume(TokenType::Eof, "");

		EndCompiler();
		return !m_Parser.HadError;
	}

	void Compiler::EndCompiler() {
		EmitByte((uint8_t)OpCode::Return);
		#ifdef DEBUG
		if (!m_Parser.HadError) {
			Disassembler::Disassemble(GetCurrentChunk(), "Code");
		}
		#endif
	}

	void Compiler::Advance() {
		m_Parser.Previous = m_Parser.Current;
		while (true) {
			m_Parser.Current = m_Scanner.ScanToken();
			if (m_Parser.Current.Type != TokenType::Error) {
				break;
			}

			Error(&m_Parser.Current, m_Parser.Current.Start);
		}
	}

	void Compiler::Consume(TokenType type, const char* msg) {
		if (m_Parser.Current.Type == type) {
			Advance();
			return;
		}

		Error(&m_Parser.Current, msg);
	}

	void Compiler::ParsePrecedence(Precedence precedence) {
		Advance();

		ParseFn prefixRule = GetRule(m_Parser.Previous.Type)->Prefix;
		if (prefixRule == NULL) {
			Error(&m_Parser.Previous, "Expect expression");
			return;
		}

		prefixRule();

		while (precedence <= GetRule(m_Parser.Current.Type)->Precedence) {
			Advance();
			ParseFn infixRule = GetRule(m_Parser.Previous.Type)->Infix;
			infixRule();
		}
	}

	void Compiler::Grouping() {
		Expression();
		Consume(TokenType::RightParen, "Expect `)' after expression");
	}

	void Compiler::Expression() {
		ParsePrecedence(Precedence::Assignment);
	}

	void Compiler::Number() {
		double value = strtod(m_Parser.Previous.Start, NULL);
		EmitConstant(value);
	}

	void Compiler::Binary() {
		TokenType operatorType = m_Parser.Previous.Type;
		ParseRule* rule = GetRule(operatorType);
		ParsePrecedence((Precedence)((int)rule->Precedence + 1));

		switch (operatorType) {
			case TokenType::Plus:	EmitByte((uint8_t)OpCode::Add); break;
			case TokenType::Minus:	EmitByte((uint8_t)OpCode::Subtract); break;
			case TokenType::Star:	EmitByte((uint8_t)OpCode::Multiply); break;
			case TokenType::Slash:	EmitByte((uint8_t)OpCode::Divide); break;
			default:
				fprintf(stderr, "Invalid operator `%d'", operatorType);
				return;
		}
	}

	void Compiler::Unary() {
		TokenType operatorType = m_Parser.Previous.Type;
		Expression();
		ParsePrecedence(Precedence::Unary);

		switch (operatorType) {
			case TokenType::Minus: EmitByte((uint8_t)OpCode::Negate);
			default:
				fprintf(stderr, "Invalid operator `%d'", operatorType);
				return;
		}
	}

	void Compiler::EmitByte(uint8_t byte) {
		GetCurrentChunk()->Write(byte, m_Parser.Previous.Line);
	}

	void Compiler::EmitBytes(uint8_t byte1, uint8_t byte2) {
		EmitByte(byte1);
		EmitByte(byte2);
	}

	void Compiler::EmitConstant(double value) {
		EmitBytes((uint8_t)OpCode::Constant, MakeConstant(value));
	}

	uint8_t Compiler::MakeConstant(double value) {
		return GetCurrentChunk()->AddConstant(0, {}, { value });
	}

	std::shared_ptr<Chunk> Compiler::GetCurrentChunk() {
		return m_CompilingChunk;
	}

	void Compiler::Error(Token* token, const char* msg) {
		if (m_Parser.InPanicMode) return;
		m_Parser.HadError = true;
		m_Parser.InPanicMode = true;

		fprintf(stderr, "[line %d] Error", token->Line);
		if (token->Type == TokenType::Eof) {
			fprintf(stderr, " at end: %s\n", msg);
		} else if (token->Type != TokenType::Error) {
			fprintf(stderr, " at `%.*s': %s\n", token->Length, token->Start, msg);
		}
	}

}

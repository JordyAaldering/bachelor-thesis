#include "Compiler.h"

namespace Lang {

	Compiler::Compiler(const char* source)
		: m_Scanner(source) {
	}

	bool Compiler::Compile(std::shared_ptr<Chunk> chunk) {
		m_CompilingChunk = chunk;
		m_Parser.HadError = false;
		m_Parser.InPanicMode = false;

		Advance();
		Expression();
		Consume(TokenType::Eof);

		EmitByte((uint8_t)OpCode::Return);
		return !m_Parser.HadError;
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

	void Compiler::Grouping() {
		Expression();
		Consume(TokenType::RightParen, "Expect `)' after expression\n");
	}

	void Compiler::Expression() {

	}

	void Compiler::Number() {
		double value = strtod(m_Parser.Previous.Start, NULL);
		EmitConstant(value);
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

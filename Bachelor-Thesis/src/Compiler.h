#pragma once

#include "Base.h"
#include "Scanner.h"
#include "Chunk.h"

namespace Lang {

	enum class Precedence {
		None, Assignment, Or, And,
		Equality, Comparison, Term,
		Factor, Unary, Call, Primary
	};

	class Compiler {
	public:
		Compiler(const char* source);
		bool Compile(std::shared_ptr<Chunk> chunk);

	private:
		void Advance();
		void Consume(TokenType type, const char* msg);
		void ParsePrecedence(Precedence precedence);

		void Grouping();
		void Expression();
		void Number();
		void Binary();
		void Unary();

		void EmitByte(uint8_t byte);
		void EmitBytes(uint8_t byte1, uint8_t byte2);
		void EmitConstant(double value);
		uint8_t MakeConstant(double value);

		std::shared_ptr<Chunk> GetCurrentChunk();

		void Error(Token* token, const char* msg);

	private:
		Scanner m_Scanner;
		std::shared_ptr<Chunk> m_CompilingChunk;

		struct Parser {
			Token Current;
			Token Previous;
			bool HadError;
			bool InPanicMode;
		} m_Parser;
	};

}

#pragma once

#include "Base.h"
#include "Scanner.h"
#include "Core/Chunk.h"

namespace Lang {

	enum class Precedence {
		None, Assignment, Or, And,
		Equality, Comparison, Term,
		Factor, Unary, Call, Primary
	};

	struct Parser {
		Token Current;
		Token Previous;
		bool HadError;
		bool InPanicMode;
	};

	typedef void (*ParseFn)();
	struct ParseRule {
		ParseFn Prefix;
		ParseFn Infix;
		Precedence Precedence;
	};

	class Compiler {
	public:
		static bool Compile(const char* source, std::shared_ptr<Chunk> chunk);
		static void EndCompiler();

	private:
		static void Advance();
		static void Consume(TokenType type, const char* msg);
		static void ParsePrecedence(Precedence precedence);

		static void Grouping();
		static void Expression();
		static void Number();
		static void Binary();
		static void Unary();

		static void EmitByte(uint8_t byte);
		static void EmitBytes(uint8_t byte1, uint8_t byte2);
		static void EmitConstant(double value);
		static uint8_t MakeConstant(double value);

		static std::shared_ptr<Chunk> GetCurrentChunk();
		static ParseRule* GetRule(TokenType type);

		static void Error(Token* token, const char* msg);

	private:
		static Scanner m_Scanner;
		static std::shared_ptr<Chunk> m_CompilingChunk;
		static Parser m_Parser;

		static ParseRule m_Rules[];
	};

}

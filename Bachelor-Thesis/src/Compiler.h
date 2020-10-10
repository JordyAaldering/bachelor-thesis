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

	typedef void (*ParseFn)(bool canAssign);
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
		static bool Check(TokenType type);
		static bool Match(TokenType type);
		static void Consume(TokenType type, const char* msg);
		static void ParsePrecedence(Precedence precedence);

		static void Grouping(bool canAssign);
		static void Declaration();
		static void VarDeclaration();
		static void Expression();
		static void Statement();
		static void ReturnStatement();
		static void ExpressionStatement();
		static void Variable(bool canAssign);
		static void NamedVariable(Token name, bool canAssign);
		static void Vector(bool canAssign);
		static void Number(bool canAssign);
		static void Binary(bool canAssign);
		static void Unary(bool canAssign);

		static void EmitByte(uint16_t byte);
		static void EmitBytes(uint16_t byte1, uint16_t byte2);
		static void EmitConstant(Value value);
		static uint16_t MakeConstant(Value value);

		static std::shared_ptr<Chunk> GetCurrentChunk();
		static ParseRule* GetRule(TokenType type);

		static void Synchronize();
		static void Error(Token* token, const char* msg);

	private:
		static Scanner m_Scanner;
		static std::shared_ptr<Chunk> m_CompilingChunk;
		static Parser m_Parser;

		static ParseRule m_Rules[];
	};

}

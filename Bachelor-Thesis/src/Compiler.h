#pragma once

#include "Base.h"
#include "Parser.h"
#include "Core/Chunk.h"

namespace Lang {

	enum class Precedence {
		None, Assignment, Or, And,
		Equality, Comparison, Term,
		Factor, Unary, Call, Primary
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
		static void Grouping(bool canAssign);
		static void Expression();
		static void DimExpr(bool canAssign);
		static void ShapeExpr(bool canAssign);
		static void SelExpr(bool canAssign);
		static void LetExpr(bool canAssign);
		static void IfExpr(bool canAssign);

		static void Variable(bool canAssign);
		static void Vector(bool canAssign);
		static void Number(bool canAssign);
		static void Binary(bool canAssign);
		static void Unary(bool canAssign);

		static void ParsePrecedence(Precedence precedence);

		static void EmitByte(uint8_t byte);
		static void EmitBytes(uint8_t byte1, uint8_t byte2);
		static void EmitConstant(Value value);
		static uint8_t MakeConstant(Value value);

		static int EmitJump(OpCode opCode);
		static void PatchJump(int offset);

		static ParseRule* GetRule(TokenType type);

	private:
		static std::shared_ptr<Parser> m_Parser;
		static std::shared_ptr<Chunk> m_Chunk;
		static ParseRule m_ParseRules[];
	};

}

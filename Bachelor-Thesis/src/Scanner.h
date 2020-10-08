#pragma once

#include "Base.h"

namespace Lang {

	enum class TokenType {
		LeftParen, RightParen,
		LeftBrace, RightBrace,
		Comma, Dot, Semicolon,
		Plus, Minus, Star, Slash,

		Bang, BangEqual,
		Equal, EqualEqual,
		Greater, GreaterEqual,
		Less, LessEqual,

		Number,
		Identifier,

		Var, Null, True, False, 
		If, Else, And, Or,
		Fun, Print, Return,
		Sel, Shape, Dim,

		Error,
		Eof,
	};

	struct Token {
		TokenType Type;
		const char* Start;
		uint16_t Length;
		uint16_t Line;
	};

	class Scanner {
	public:
		Scanner(const char* source);

		Token ScanToken();

	private:
		const char* m_Start;
		const char* m_Current;
		uint16_t m_Line;
	};

}

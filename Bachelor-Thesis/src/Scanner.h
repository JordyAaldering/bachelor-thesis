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
		And, Or,
		Number, Identifier,
		Var, Fun, If, Else,
		Dim, Shape, Sel,
		Print, Return,
		Error, Eof,
	};

	struct Token {
		TokenType Type;
		const char* Start;
		uint8_t Length;
		uint16_t Line;
	};

	class Scanner {
	public:
		Scanner(const char* source);

		Token ScanToken();

	private:
		Token MakeToken(TokenType type);
		Token ErrorToken(const char* message);
		Token MakeNumber();
		Token MakeIdentifier();
		TokenType GetIdentifierType();

		char Peek();
		char PeekNext();
		char Advance();
		bool Match(char expected);
		bool IsDigit(char c);
		bool IsAlpha(char c);
		bool IsAtEnd();

		void SkipWhitespace();

	private:
		const char* m_Start;
		const char* m_Current;
		uint16_t m_Line;
	};

}

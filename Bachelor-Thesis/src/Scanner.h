#pragma once

#include "Base.h"

namespace Lang {

	enum class TokenType {
		LeftParen, RightParen,
		LeftBrace, RightBrace,
		LeftSquare, RightSquare,
		Dot, Comma, Semicolon,
		Plus, Minus, Star, Slash,
		Equal, EqualEqual,
		Bang, BangEqual,
		Greater, GreaterEqual,
		Less, LessEqual,
		And, Or,
		Number, Identifier,
		Function, Main,
		Dim, Shape, Sel,
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
		Token ErrorToken(const char* msg);
		Token MakeNumber();
		Token MakeIdentifier();
		TokenType GetIdentifierType();

		char Peek();
		char PeekNext();
		char Advance();
		bool Match(char expected);
		bool IsDigit(char c);
		bool IsLetter(char c);
		bool IsAtEnd();

		void SkipWhitespace();

	private:
		const char* m_Start;
		const char* m_Current;
		uint16_t m_Line;
	};

}

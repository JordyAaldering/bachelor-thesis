#include "Scanner.h"

namespace Lang {

	Scanner::Scanner(const char* source)
		: m_Start(source), m_Current(source), m_Line(1) {
	}

	Token Scanner::ScanToken() {
		SkipWhitespace();

		m_Start = m_Current;
		if (IsAtEnd()) return MakeToken(TokenType::Eof);

		char c = Advance();
		if (IsDigit(c)) return MakeNumber();
		if (IsLetter(c)) return MakeIdentifier();

		switch (c) {
			case '(': return MakeToken(TokenType::LeftParen);
			case ')': return MakeToken(TokenType::RightParen);
			case '[': return MakeToken(TokenType::LeftSquare);
			case ']': return MakeToken(TokenType::RightSquare);
			case ',': return MakeToken(TokenType::Comma);

			case '+': return MakeToken(TokenType::Plus);
			case '-': return MakeToken(TokenType::Minus);
			case '*': return MakeToken(TokenType::Star);
			case '/': return MakeToken(TokenType::Slash);

			case '=': return MakeToken(Match('=') ? TokenType::EqualEqual	: TokenType::Equal);
			case '!': return MakeToken(Match('=') ? TokenType::BangEqual	: TokenType::Bang);
			case '>': return MakeToken(Match('=') ? TokenType::GreaterEqual : TokenType::Greater);
			case '<': return MakeToken(Match('=') ? TokenType::LessEqual	: TokenType::Less);
			case '&': return Match('&') ? MakeToken(TokenType::And) : ErrorToken("Expected `&&'");
			case '|': return Match('|') ? MakeToken(TokenType::Or) :  ErrorToken("Expected `||'");
		}

		return ErrorToken("Unexpected character");
	}

	Token Scanner::MakeToken(TokenType type) {
		return { type, m_Start, (uint8_t)(m_Current - m_Start), m_Line };
	}

	Token Scanner::ErrorToken(const char* msg) {
		return { TokenType::Error, msg, (uint8_t)strlen(msg), m_Line };
	}

	Token Scanner::MakeNumber() {
		while (IsDigit(Peek())) {
			Advance();
		}

		if (Match('.')) {
			while (IsDigit(Peek())) {
				Advance();
			}
		}

		return MakeToken(TokenType::Number);
	}

	Token Scanner::MakeIdentifier() {
		while (IsLetter(Peek()) || IsDigit(Peek())) {
			Advance();
		}

		return MakeToken(GetIdentifierType());
	}

	TokenType Scanner::GetIdentifierType() {
		if (strncmp(m_Start, "function", 8) == 0)	return TokenType::Function;
		if (strncmp(m_Start, "dim", 3) == 0)		return TokenType::Dim;
		if (strncmp(m_Start, "shape", 5) == 0)		return TokenType::Shape;
		if (strncmp(m_Start, "sel", 3) == 0)		return TokenType::Sel;
		if (strncmp(m_Start, "let", 3) == 0)		return TokenType::Let;
		if (strncmp(m_Start, "in", 2) == 0)			return TokenType::In;
		if (strncmp(m_Start, "if", 2) == 0)			return TokenType::If;
		if (strncmp(m_Start, "then", 4) == 0)		return TokenType::Then;
		if (strncmp(m_Start, "else", 4) == 0)		return TokenType::Else;

		return TokenType::Identifier;
	}

	char Scanner::Peek() {
		return *m_Current;
	}

	char Scanner::PeekNext() {
		if (IsAtEnd()) return '\0';
		return m_Current[1];
	}

	char Scanner::Advance() {
		m_Current++;
		return m_Current[-1];
	}

	bool Scanner::Match(char expected) {
		if (IsAtEnd() || Peek() != expected) {
			return false;
		}

		m_Current++;
		return true;
	}

	bool Scanner::IsDigit(char c) {
		return c >= '0' && c <= '9';
	}

	bool Scanner::IsLetter(char c) {
		return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';
	}

	bool Scanner::IsAtEnd() {
		return Peek() == '\0';
	}

	void Scanner::SkipWhitespace() {
		while (true) {
			char c = Peek();
			switch (c) {
				case ' ':
				case '\r':
				case '\t':
					Advance();
					break;
				case '\n':
					m_Line++;
					Advance();
					break;
				default:
					return;
			}
		}
	}

}

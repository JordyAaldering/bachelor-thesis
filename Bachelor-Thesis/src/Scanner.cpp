#include "Scanner.h"

namespace Lang {

	Scanner::Scanner(const char* source)
		: m_Start(source), m_Current(source), m_Line(1) {
	}

	Token Scanner::ScanToken() {
		m_Start = m_Current;
		if (IsAtEnd()) {
			return MakeToken(TokenType::Eof);
		}

		return ErrorToken("Unexpected character");
	}

	Token Scanner::MakeToken(TokenType type) {
		return {
			type,
			m_Start,
			(uint8_t)(m_Current - m_Start),
			m_Line
		};
	}

	Token Scanner::ErrorToken(const char* message) {
		return {
			TokenType::Error,
			message,
			(uint8_t)strlen(message),
			m_Line
		};
	}

	bool Scanner::IsAtEnd() {
		return *m_Current == '\0';
	}

}

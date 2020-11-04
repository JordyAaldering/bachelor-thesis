#pragma once

#include "Base.h"
#include "Scanner.h"

namespace Lang {

	class Parser {
	public:
		Parser(const char* source);
		bool Parse();

		Token Peek();
		Token PeekNext();
		Token Advance();
		bool Check(TokenType type);
		bool Match(TokenType type);
		void Consume(TokenType type, const char* msg);
		
		void Error(Token* token, const char* msg);
		void Synchronize();
		void Print();

	private:

	private:
		Scanner m_Scanner;
		std::vector<Token> m_Tokens;
		int m_ReadIndex = -1;

		bool m_HadError = false;
		bool m_InPanicMode = false;
	};

}

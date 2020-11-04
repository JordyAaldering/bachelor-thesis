#pragma once

#include "Base.h"
#include "Scanner.h"

namespace Lang {

	class Parser {
	public:
		bool Parse(const char* source);
		void Error(Token* token, const char* msg);
		void Print();
		Token Get(int index) { return m_Tokens[index]; }
		bool HadError() { return m_HadError; }

	private:
		std::vector<Token> m_Tokens;
		bool m_HadError = false;
	};

}

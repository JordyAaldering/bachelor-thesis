#include "Parser.h"

namespace Lang {

	bool Parser::Parse(const char* source) {
		Scanner scanner(source);
		Token token;
		m_Tokens = std::vector<Token>();

		do {
			token = scanner.ScanToken();

			while (token.Type == TokenType::Error) {
				Error(&token, token.Start);
				token = scanner.ScanToken();
			}
			
			m_Tokens.push_back(token);
		} while (token.Type != TokenType::Eof);

		return !m_HadError;
	}

	void Parser::Error(Token* token, const char* msg) {
		if (m_HadError) return;
		m_HadError = true;

		fprintf(stderr, "[line %d] Error", token->Line);
		if (token->Type == TokenType::Eof) {
			fprintf(stderr, " at end: %s\n", msg);
		} else if (token->Type != TokenType::Error) {
			fprintf(stderr, " at `%.*s': %s\n", token->Length, token->Start, msg);
		}
	}

	void Parser::Print() {
		for (Token t : m_Tokens) {
			printf("%d, %.*s, %d\n", (int)t.Type, t.Length, t.Start, t.Line);
		}
	}

}

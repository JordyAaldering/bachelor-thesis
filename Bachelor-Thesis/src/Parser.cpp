#include "Parser.h"

namespace Lang {

	Parser::Parser(const char* source)
		: m_Tokens(), m_Scanner(source) {
	}

	bool Parser::Parse() {
		Token token;
		do {
			token = m_Scanner.ScanToken();
			while (token.Type == TokenType::Error) {
				Error(&token, token.Start);
				token = m_Scanner.ScanToken();
			}
			
			m_Tokens.push_back(token);
		} while (token.Type != TokenType::Eof);

		return !m_HadError;
	}

	Token Parser::Peek() {
		return m_Tokens[m_ReadIndex];
	}

	Token Parser::PeekNext() {
		return m_Tokens[m_ReadIndex + 1];
	}

	Token Parser::Advance() {
		return m_Tokens[++m_ReadIndex];
	}

	bool Parser::Check(TokenType type) {
		return m_Tokens[m_ReadIndex + 1].Type == type;
	}

	bool Parser::Match(TokenType type) {
		if (!Check(type)) return false;
		Advance();
		return true;
	}

	void Parser::Consume(TokenType type, const char* msg) {
		if (Match(type)) return;
		Error(&m_Tokens[m_ReadIndex + 1], msg);
	}

	void Parser::Error(Token* token, const char* msg) {
		if (m_InPanicMode) return;
		m_HadError = true;
		m_InPanicMode = true;

		fprintf(stderr, "[line %d] Error", token->Line);
		if (token->Type == TokenType::Eof) {
			fprintf(stderr, " at end: %s\n", msg);
		} else if (token->Type != TokenType::Error) {
			fprintf(stderr, " at `%.*s': %s\n", token->Length, token->Start, msg);
		}
	}

	void Parser::Synchronize() {
		if (!m_InPanicMode) return;
		m_InPanicMode = false;

		while (!Check(TokenType::Eof)) {
			switch (PeekNext().Type) {
				case TokenType::Function:
				case TokenType::Dim:
				case TokenType::Shape:
				case TokenType::Sel:
					return;
			}
		}
	}

	void Parser::Print() {
		for (Token t : m_Tokens) {
			printf("%-12s %.*s\n", ToString(t.Type), t.Length, t.Start);
		}
	}

}

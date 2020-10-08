#include "Compiler.h"
#include "Scanner.h"

namespace Lang {

	void Compiler::Compile(const char* source) {
		Scanner scanner(source);

		uint16_t line = 0;
		while (true) {
			Token token = ScanToken();
			if (token.Type == TokenType::EOF) {
				break;
			}
		}
	}

}

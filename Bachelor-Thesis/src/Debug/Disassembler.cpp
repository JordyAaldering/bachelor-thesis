#include "Disassembler.h"

namespace Lang {

	void Disassembler::Disassemble(Chunk* chunk, const char* name) {
		printf("== %s ==\n", name);

		int offset = 0;
		while (offset < chunk->Code.size()) {
			offset = DisassembleInstruction(chunk, offset);
		}
	}

	int Disassembler::DisassembleInstruction(Chunk* chunk, int offset) {
		printf("%04d ", offset);

		OpCode instruction = chunk->Code[offset];
		switch (instruction) {
			case OpCode::Return: return SimpleInstruction("Return", offset);
		}

		fprintf(stderr, "Unknown OpCode `%d'.\n", instruction);
		return offset + 1;
	}

	int Disassembler::SimpleInstruction(const char* name, int offset) {
		printf("%s\n", name);
		return offset + 1;
	}

}

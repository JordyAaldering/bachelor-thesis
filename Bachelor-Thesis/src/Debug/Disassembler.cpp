#include "Disassembler.h"

namespace Lang {

	void Disassembler::Disassemble(std::shared_ptr<Chunk> chunk, const char* name) {
		printf("== %s ==\n", name);
		printf("Offset Line OpCode\n");

		int offset = 0;
		while (offset < chunk->Code.size()) {
			offset = DisassembleInstruction(chunk, offset);
		}
	}

	int Disassembler::DisassembleInstruction(std::shared_ptr<Chunk> chunk, int offset) {
		printf("%04d   ", offset);
		if (offset == 0 || chunk->Lines[offset] != chunk->Lines[offset - 1]) {
			printf("%4d ", chunk->Lines[offset]);
		} else {
			printf("   | ");
		}

		OpCode instruction = (OpCode)chunk->Code[offset];
		switch (instruction) {
			case OpCode::Constant: return ConstantInstruction("Constant", chunk, offset);
			case OpCode::Return: return SimpleInstruction("Return", offset);
		}

		fprintf(stderr, "Unknown OpCode `%d'.\n", instruction);
		return offset + 1;
	}

	int Disassembler::ConstantInstruction(const char* name, std::shared_ptr<Chunk> chunk, int offset) {
		uint8_t index = chunk->Code[offset + 1];
		printf("%-12s %4d `", name, index);
		chunk->Constants[index].Print();
		printf("'\n");
		return offset + 2;
	}

	int Disassembler::SimpleInstruction(const char* name, int offset) {
		printf("%s\n", name);
		return offset + 1;
	}

}

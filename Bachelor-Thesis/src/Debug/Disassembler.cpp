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
			case OpCode::Constant:		return ConstantInstruction("Constant", chunk, offset);

			case OpCode::SetVariable:	return VariableInstruction("Set Variable", chunk, offset);
			case OpCode::GetVariable:	return VariableInstruction("Get Variable", chunk, offset);
			case OpCode::PopVariable:	return SimpleInstruction("Pop Variable", offset);

			case OpCode::Not:			return SimpleInstruction("Not", offset);
			case OpCode::Equal:			return SimpleInstruction("Equal", offset);
			case OpCode::NotEqual:		return SimpleInstruction("Not Equal", offset);
			case OpCode::Greater:		return SimpleInstruction("Greater", offset);
			case OpCode::GreaterEqual:	return SimpleInstruction("Greater Equal", offset);
			case OpCode::Less:			return SimpleInstruction("Less", offset);
			case OpCode::LessEqual:		return SimpleInstruction("Less Equal", offset);
			case OpCode::And:			return SimpleInstruction("And", offset);
			case OpCode::Or:			return SimpleInstruction("Or", offset);

			case OpCode::Negate:		return SimpleInstruction("Negate", offset);
			case OpCode::Add:			return SimpleInstruction("Add", offset);
			case OpCode::Subtract:		return SimpleInstruction("Subtract", offset);
			case OpCode::Multiply:		return SimpleInstruction("Multiply", offset);
			case OpCode::Divide:		return SimpleInstruction("Divide", offset);

			case OpCode::Return:		return SimpleInstruction("Return", offset);

			default:
				fprintf(stderr, "Unknown OpCode `%d'\n", instruction);
				return offset + 1;
		}
	}

	int Disassembler::ConstantInstruction(const char* name, std::shared_ptr<Chunk> chunk, int offset) {
		uint16_t index = chunk->Code[offset + 1];
		printf("%-12s %4d ", name, index);
		chunk->Constants[index].Print();
		return offset + 2;
	}

	int Disassembler::VariableInstruction(const char* name, std::shared_ptr<Chunk> chunk, int offset) {
		uint16_t index = chunk->Code[offset + 1];
		printf("%-12s %4d %s\n", name, index, chunk->Variables[index].c_str());
		return offset + 2;
	}

	int Disassembler::SimpleInstruction(const char* name, int offset) {
		printf("%s\n", name);
		return offset + 1;
	}

}

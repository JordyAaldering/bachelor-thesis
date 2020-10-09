#pragma once

#include "Base.h"
#include "Core/Chunk.h"

namespace Lang {

	class Disassembler {
	public:
		static void Disassemble(std::shared_ptr<Chunk> chunk, const char* name);
		static int DisassembleInstruction(std::shared_ptr<Chunk> chunk, uint32_t offset);

	private:
		static int ConstantInstruction(const char* name, std::shared_ptr<Chunk> chunk, uint32_t offset);
		static int SimpleInstruction(const char* name, uint32_t offset);
	};

}

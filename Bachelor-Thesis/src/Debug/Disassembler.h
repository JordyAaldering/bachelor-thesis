#pragma once

#include "Base.h"
#include "Chunk.h"

namespace Lang {

	class Disassembler {
	public:
		static void Disassemble(Chunk* chunk, const char* name);
		static int DisassembleInstruction(Chunk* chunk, int offset);

	private:
		static int SimpleInstruction(const char* name, int offset);
	};

}

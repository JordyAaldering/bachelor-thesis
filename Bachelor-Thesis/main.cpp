#include "Chunk.h"
#include "Debug/Disassembler.h"

using namespace Lang;

int main(int argc, char* argv[]) {
	Chunk chunk;
	chunk.Write(OpCode::Return);
	Disassembler::Disassemble(&chunk, "Test");

	return 0;
}

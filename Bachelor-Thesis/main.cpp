#include "Chunk.h"
#include "Debug/Disassembler.h"

using namespace Lang;

int main(int argc, char* argv[]) {
	Chunk chunk;

	uint8_t dim = 1;
	uint8_t shape[] = { 2 };
	double values[] = { 7.0, 6.0 };
	int index = chunk.AddConstant(dim, shape, values);
	
	chunk.Write((uint8_t)OpCode::Constant);
	chunk.Write(index);
	chunk.Write((uint8_t)OpCode::Return);
	Disassembler::Disassemble(&chunk, "Test");

	return 0;
}

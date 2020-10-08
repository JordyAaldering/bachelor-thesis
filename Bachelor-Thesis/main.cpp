#include "Chunk.h"
#include "Debug/Disassembler.h"

using namespace Lang;

int main(int argc, char* argv[]) {
	auto chunk = std::make_shared<Chunk>();

	int index = chunk->AddConstant(1, { 2 }, { 7.0, 6.0 });	
	chunk->Write((uint8_t)OpCode::Constant, 123);
	chunk->Write(index, 123);
	chunk->Write((uint8_t)OpCode::Return, 124);
	Disassembler::Disassemble(chunk, "Test");

	return 0;
}

#include "Runtime.h"
#include "Debug/Disassembler.h"

using namespace Lang;

int main(int argc, char* argv[]) {
	auto chunk = std::make_shared<Chunk>();

	int index = chunk->AddConstant(1, { 2 }, { 7.0, 6.0 });	
	chunk->Write((uint8_t)OpCode::Constant, 123);
	chunk->Write(index, 123);

	index = chunk->AddConstant(1, { 2 }, { 2.0, 2.0 });
	chunk->Write((uint8_t)OpCode::Constant, 124);
	chunk->Write(index, 124);

	chunk->Write((uint8_t)OpCode::Divide, 123);
	chunk->Write((uint8_t)OpCode::Negate, 124);

	chunk->Write((uint8_t)OpCode::Return, 125);
	
	Runtime vm;
	vm.Interpret(chunk);

	return 0;
}

#include "Chunk.h"

namespace Lang {
	
	Chunk::Chunk()
		: Code(), Lines(), Constants() {}
	
	void Chunk::Write(uint8_t byte, uint16_t line) {
		Code.push_back(byte);
		Lines.push_back(line);
	}

	uint8_t Chunk::AddConstant(Value value) {
		Constants.push_back(value);
		return Constants.size() - 1;
	}

}

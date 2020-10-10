#include "Chunk.h"

namespace Lang {
	
	void Chunk::Write(uint8_t byte, uint16_t line) {
		Code.push_back(byte);
		Lines.push_back(line);
	}

	uint8_t Chunk::AddConstant(Value value) {
		Constants.push_back(value);
		return (uint8_t)(Constants.size() - 1);
	}

}

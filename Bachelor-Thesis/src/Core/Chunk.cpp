#include "Chunk.h"

namespace Lang {
	
	void Chunk::Write(uint16_t byte, uint16_t line) {
		Code.push_back(byte);
		Lines.push_back(line);
	}

	uint16_t Chunk::AddConstant(Value value) {
		Constants.push_back(value);
		return (uint16_t)(Constants.size() - 1);
	}

}

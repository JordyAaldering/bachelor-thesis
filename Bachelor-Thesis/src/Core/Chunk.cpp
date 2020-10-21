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

	uint8_t Chunk::AddVariable(std::string name) {
		Variables.push_back(name);
		return (uint8_t)(Variables.size() - 1);
	}

	uint8_t Chunk::GetVariable(std::string name) {
		for (int i = Variables.size() - 1; i >= 0; i--) {
			if (Variables[i] == name) {
				return (uint8_t)i;
			}
		}
		return 0;
	}

}

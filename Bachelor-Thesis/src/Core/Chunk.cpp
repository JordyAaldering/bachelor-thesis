#include "Chunk.h"

namespace Lang {
	
	void Chunk::Write(uint8_t byte, uint16_t line) {
		Code.push_back(byte);
		Lines.push_back(line);
	}

	uint8_t Chunk::AddConstant(uint8_t dim, std::vector<uint8_t> shape, std::vector<double> values) {
		uint8_t index = (uint8_t)Constants.size();
		Constants.emplace_back(dim, shape, values);
		return index;
	}

}

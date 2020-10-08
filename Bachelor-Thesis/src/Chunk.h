#pragma once

#include "Base.h"
#include "Value.h"

namespace Lang {

	enum class OpCode {
		Constant,
		Negate,
		Add, Subtract,
		Multiply, Divide,
		Return,
	};

	struct Chunk {
		std::vector<uint8_t> Code;
		std::vector<uint16_t> Lines;
		std::vector<Value> Constants;

		/// <summary>Writes a byte to the bytcode.</summary>
		void Write(uint8_t byte, uint16_t line) {
			Code.push_back(byte);
			Lines.push_back(line);
		}

		/// <summary>Adds a new value to the constants array.</summary>
		/// <returns>The index at which the value was added.</returns>
		uint8_t AddConstant(uint8_t dim, std::vector<uint8_t> shape, std::vector<double> values) {
			uint8_t index = (uint8_t)Constants.size();
			Constants.emplace_back(dim, shape, values);
			return index;
		}

	};

}

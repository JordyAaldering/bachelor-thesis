#pragma once

#include "Base.h"
#include "Value.h"

namespace Lang {

	enum class OpCode {
		Constant,
		Return,
	};

	struct Chunk {
		std::vector<uint8_t> Code;
		std::vector<int> Lines;
		std::vector<Value> Constants;

		void Write(uint8_t byte, int line) {
			Code.push_back(byte);
			Lines.push_back(line);
		}

		/// <summary>Adds a new value to the constants array.</summary>
		/// <returns>The index at which the value was added.</returns>
		int AddConstant(uint8_t dim, uint8_t* shape, double* values) {
			Constants.emplace_back(dim, shape, values);
			return Constants.size() - 1;
		}

	};

}

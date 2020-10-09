#pragma once

#include "Base.h"
#include "Value.h"

namespace Lang {

	enum class OpCode {
		Constant,
		Not, Equal, NotEqual,
		Greater, GreaterEqual,
		Less, LessEqual,
		Negate, Add, Subtract,
		Multiply, Divide,
		Return,
	};

	struct Chunk {
		std::vector<uint8_t> Code;
		std::vector<uint16_t> Lines;
		std::vector<Value> Constants;

		/// <summary>Writes a byte to the bytcode.</summary>
		void Write(uint8_t byte, uint16_t line);

		/// <summary>Adds a new value to the constants array.</summary>
		/// <returns>The index at which the value was added.</returns>
		uint8_t AddConstant(Value value);
	};

}

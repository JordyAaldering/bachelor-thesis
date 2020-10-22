#pragma once

#include "Base.h"
#include "Value.h"

namespace Lang {

	enum class OpCode {
		Constant,
		
		SetVariable,
		GetVariable,
		PopVariable,

		Jump,
		JumpIfFalse,

		Not,
		Equal, NotEqual,
		Greater, GreaterEqual,
		Less, LessEqual,
		And, Or,

		Negate,
		Add, Subtract,
		Multiply, Divide,

		Pop, Return,
	};

	struct Chunk {
		std::vector<uint8_t> Code;
		std::vector<uint16_t> Lines;
		std::vector<Value> Constants;
		std::vector<std::string> Variables;

		/// <summary>Writes a byte to the bytcode.</summary>
		void Write(uint8_t byte, uint16_t line);

		/// <summary>Adds a new value to the constants array.</summary>
		/// <returns>The index at which the value was added.</returns>
		uint8_t AddConstant(Value value);

		/// <summary>Adds a new variable name to the variables array.</summary>
		/// <returns>The index at which the variable was added.</returns>
		uint8_t AddVariable(std::string name);

		/// <summary>Gets the first index of the given variable name.</summary>
		uint8_t GetVariable(std::string name);
	};

}

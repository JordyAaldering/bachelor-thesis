#pragma once

#include "Base.h"
#include "Chunk.h"

namespace Lang {

	enum class InterpretResult {
		OK, CompileError, RuntimeError
	};

	class Runtime {
	public:
		InterpretResult Interpret(std::shared_ptr<Chunk> chunk);

	private:
		InterpretResult Run();

		uint8_t ReadByte();
		Value ReadConstant();

		void Push(Value value);
		Value Pop();

	private:
		std::shared_ptr<Chunk> m_Chunk;
		uint8_t m_CodeIndex;
		std::stack<Value> m_Stack;
	};

}

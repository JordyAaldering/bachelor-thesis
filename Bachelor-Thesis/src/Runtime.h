#pragma once

#include "Base.h"
#include "Core/Chunk.h"

namespace Lang {

	enum class InterpretResult {
		OK, CompileError, RuntimeError
	};

	class Runtime {
	public:
		Runtime();
		InterpretResult Interpret(const char* source);

	private:
		InterpretResult Run();

		uint8_t ReadByte();
		Value ReadConstant();

		void Push(Value value);
		Value Pop();

		void RuntimeError(const char* format, ...);

	private:
		std::shared_ptr<Chunk> m_Chunk;
		uint8_t m_CodeIndex;
		std::stack<Value> m_Stack;
	};

}

#pragma once

#include "Base.h"
#include "Core/Chunk.h"

namespace Lang {

	enum class InterpretResult {
		OK, CompileError, RuntimeError
	};

	class Runtime {
	public:
		static InterpretResult Interpret(const char* source);

	private:
		static InterpretResult Run();

		static uint8_t ReadByte();
		static Value ReadConstant();
		static std::string ReadVariable();
		static Value FindVariable(std::string name);

		static void Push(Value value);
		static Value Pop();

		static void RuntimeError(const char* format, ...);

	private:
		static std::shared_ptr<Chunk> m_Chunk;
		static uint16_t m_CodeIndex;
		static std::list<std::pair<std::string, Value>> m_Variables;
		static std::stack<Value> m_Stack;
	};

}

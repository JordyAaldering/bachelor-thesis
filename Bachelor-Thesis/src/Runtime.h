#pragma once

#include "Base.h"
#include "Core/Chunk.h"
#include "Core/Function.h"

namespace Lang {

	enum class InterpretResult {
		OK, CompileError, RuntimeError
	};

	struct CallFrame {
		std::shared_ptr<Function> function;
		uint8_t codeIndex;
		std::list<std::pair<std::string, Value>> variables;
	};

	class Runtime {
	public:
		static InterpretResult Interpret(const char* source);

	private:
		static InterpretResult Run();

		static uint8_t ReadByte(std::shared_ptr<CallFrame> frame);
		static uint16_t ReadShort(std::shared_ptr<CallFrame> frame);
		static Value ReadConstant(std::shared_ptr<CallFrame> frame);
		static std::string ReadVariable(std::shared_ptr<CallFrame> frame);
		static Value FindVariable(std::string name, std::shared_ptr<CallFrame> frame);

		static void Push(Value value);
		static Value Pop();

		static void RuntimeError(const char* format, ...);

	private:
		static std::stack<Value> m_Stack;
		static std::list<std::pair<const char*, Function>> m_Functions;
		static CallFrame m_Frames[FRAMES_MAX];
		static int m_FrameCount;
	};

}

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

	private:
		std::shared_ptr<Chunk> m_Chunk;
		uint8_t m_CodeIndex;
	};

}

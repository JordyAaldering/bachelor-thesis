#include "Runtime.h"

#ifdef DEBUG_TRACE
#include "Debug/Disassembler.h"
#endif // DEBUG_TRACE


namespace Lang {

	InterpretResult Runtime::Interpret(std::shared_ptr<Chunk> chunk) {
		m_Chunk = chunk;
		m_CodeIndex = 0;
		return Run();
	}

	InterpretResult Runtime::Run() {
		while (true) {
			#ifdef DEBUG_TRACE
			Disassembler::DisassembleInstruction(m_Chunk, m_CodeIndex);
			#endif // DEBUG_TRACE


			OpCode instruction = (OpCode)ReadByte();
			switch (instruction) {
				case OpCode::Constant:
					ReadConstant().Print();
					printf("\n");
					break;

				case OpCode::Return:
					return InterpretResult::OK;
			}
		}

		return InterpretResult::OK;
	}

	uint8_t Runtime::ReadByte() {
		return m_Chunk->Code[m_CodeIndex++];
	}

	Value Runtime::ReadConstant() {
		uint8_t index = ReadByte();
		return m_Chunk->Constants[index];
	}

}

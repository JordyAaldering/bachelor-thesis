#include "Runtime.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif // DEBUG


namespace Lang {

	InterpretResult Runtime::Interpret(std::shared_ptr<Chunk> chunk) {
		m_Chunk = chunk;
		m_CodeIndex = 0;
		return Run();
	}

	InterpretResult Runtime::Run() {
		while (true) {
			#ifdef DEBUG
			Disassembler::DisassembleInstruction(m_Chunk, m_CodeIndex);
			#endif // DEBUG

			OpCode instruction = (OpCode)ReadByte();
			switch (instruction) {
				case OpCode::Constant:
					m_Stack.push(ReadConstant());
					break;

				case OpCode::Return:
					m_Stack.top().Print();
					m_Stack.pop();
					printf("\n");
					return InterpretResult::OK;

				default:
					fprintf(stderr, "Unknown OpCode `%d'\n", instruction);
					return InterpretResult::RuntimeError;
			}
		}
	}

	uint8_t Runtime::ReadByte() {
		return m_Chunk->Code[m_CodeIndex++];
	}

	Value Runtime::ReadConstant() {
		uint8_t index = ReadByte();
		return m_Chunk->Constants[index];
	}

}

#include "Runtime.h"
#include "Compiler.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif // DEBUG

namespace Lang {

	InterpretResult Runtime::Interpret(const char* source) {
		Compiler::Compile(source);
		return InterpretResult::OK;
	}

	InterpretResult Runtime::Run() {
		while (true) {
			#ifdef DEBUG
			Disassembler::DisassembleInstruction(m_Chunk, m_CodeIndex);
			#endif

			OpCode instruction = (OpCode)ReadByte();
			switch (instruction) {
				case OpCode::Constant:
					Push(ReadConstant());
					break;

				case OpCode::Negate:
					Push(-Pop());
					break;
				case OpCode::Add: {
					Value r = Pop();
					Value l = Pop();
					Push(l + r);
					break;
				}
				case OpCode::Subtract: {
					Value r = Pop();
					Value l = Pop();
					Push(l - r);
					break;
				}
				case OpCode::Multiply: {
					Value r = Pop();
					Value l = Pop();
					Push(l * r);
					break;
				}
				case OpCode::Divide: {
					Value r = Pop();
					Value l = Pop();
					Push(l / r);
					break;
				}

				case OpCode::Return:
					Pop().Print();
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

	void Runtime::Push(Value value) {
		m_Stack.push(value);
	}

	Value Runtime::Pop() {
		Value v = m_Stack.top();
		m_Stack.pop();
		return v;
	}

}

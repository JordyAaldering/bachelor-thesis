#include "Runtime.h"
#include "Compiler.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif

namespace Lang {

	std::stack<Value> Runtime::m_Stack;
	std::list<std::pair<const char*, Function>> Runtime::m_Functions;
	CallFrame Runtime::m_Frames[FRAMES_MAX];
	int Runtime::m_FrameCount;

	InterpretResult Runtime::Interpret(const char* source) {
		auto function = Compiler::Compile(source, FunctionType::Script);
		if (function == nullptr) {
			return InterpretResult::CompileError;
		}

		m_Functions.emplace_back(function->name, function);
		CallFrame* frame = &m_Frames[m_FrameCount++];
		frame->function = function;
		frame->codeIndex = 0;

		m_Stack = std::stack<Value>();
		m_FrameCount = 0;

		InterpretResult res = Run();
		return res;
	}

	InterpretResult Runtime::Run() {
		#define UNARY_OP(op) Push(op Pop())
		#define BINARY_OP(op) { Value r = Pop(); Push(Pop() op r); }

		std::shared_ptr<CallFrame> frame(&m_Frames[m_FrameCount - 1]);

		while (true) {
			#ifdef DEBUG
			Disassembler::DisassembleInstruction(
				std::shared_ptr<Chunk>(&frame->function->chunk), frame->codeIndex);
			#endif

			OpCode instruction = (OpCode)ReadByte(frame);
			switch (instruction) {
				case OpCode::Constant:
					Push(ReadConstant(frame));
					break;
				case OpCode::SetVariable: {
					std::string name = ReadVariable(frame);
					Value value = m_Stack.top();
					frame->variables.emplace_front(name, value);
					break;
				}
				case OpCode::GetVariable: {
					std::string name = ReadVariable(frame);
					Push(FindVariable(name, frame));
					break;
				}
				case OpCode::PopVariable:
					frame->variables.erase(frame->variables.begin());
					break;

				case OpCode::Jump: {
					uint16_t offset = ReadShort(frame);
					frame->codeIndex += offset;
					break;
				}
				case OpCode::JumpIfFalse: {
					uint16_t offset = ReadShort(frame);
					if (!(bool)m_Stack.top()) frame->codeIndex += offset;
					break;
				}

				case OpCode::Not:			UNARY_OP(!); break;
				case OpCode::Equal:			BINARY_OP(==); break;
				case OpCode::NotEqual:		BINARY_OP(!=); break;
				case OpCode::Greater:		BINARY_OP(>); break;
				case OpCode::GreaterEqual:	BINARY_OP(>=); break;
				case OpCode::Less:			BINARY_OP(<); break;
				case OpCode::LessEqual:		BINARY_OP(<=); break;

				case OpCode::Negate:		UNARY_OP(-); break;
				case OpCode::Add:			BINARY_OP(+); break;
				case OpCode::Subtract:		BINARY_OP(-); break;
				case OpCode::Multiply:		BINARY_OP(*); break;
				case OpCode::Divide:		BINARY_OP(/); break;
					
				case OpCode::Pop: Pop(); break;
				case OpCode::Return:
					Pop().Print();
					return InterpretResult::OK;

				default:
					fprintf(stderr, "Unknown OpCode `%d'\n", instruction);
					return InterpretResult::RuntimeError;
			}
		}

		#undef UNARY_OP
		#undef BINARY_OP
	}

	uint8_t Runtime::ReadByte(std::shared_ptr<CallFrame> frame) {
		return frame->function->chunk.Code[frame->codeIndex++];
	}

	uint16_t Runtime::ReadShort(std::shared_ptr<CallFrame> frame) {
		uint8_t hi = frame->function->chunk.Code[frame->codeIndex] << 8;
		uint8_t lo = frame->function->chunk.Code[frame->codeIndex + 1];
		frame->codeIndex += 2;
		return (uint16_t)(hi | lo);
	}

	Value Runtime::ReadConstant(std::shared_ptr<CallFrame> frame) {
		uint8_t index = ReadByte(frame);
		return frame->function->chunk.Constants[index];
	}

	std::string Runtime::ReadVariable(std::shared_ptr<CallFrame> frame) {
		uint8_t index = ReadByte(frame);
		return frame->function->chunk.Variables[index];
	}

	Value Runtime::FindVariable(std::string name, std::shared_ptr<CallFrame> frame) {
		for (auto var : frame->variables) {
			if (name == var.first) {
				return var.second;
			}
		}
		return 0;
	}

	void Runtime::Push(Value value) {
		m_Stack.push(value);
	}

	Value Runtime::Pop() {
		Value v = m_Stack.top();
		m_Stack.pop();
		return v;
	}

	void Runtime::RuntimeError(const char* format, ...) {
		va_list args;
		va_start(args, format);
		vfprintf(stderr, format, args);
		va_end(args);
		fputs("\n", stderr);

		CallFrame* frame = &m_Frames[m_FrameCount - 1];
		uint16_t line = frame->function->chunk.Lines[frame->codeIndex];
		fprintf(stderr, "[line %d] in script\n", line);
	}

}

#include "Runtime.h"
#include "Compiler.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif

namespace Lang {

	Runtime::Runtime()
		: m_Chunk(), m_CodeIndex(0), m_Stack() {}

	InterpretResult Runtime::Interpret(const char* source) {
		m_Chunk = std::make_shared<Chunk>();
		if (!Compiler::Compile(source, m_Chunk)) {
			return InterpretResult::CompileError;
		}

		m_CodeIndex = 0;
		InterpretResult res = Run();
		return res;
	}

	InterpretResult Runtime::Run() {
		#define UNARY_OP(op) Push(op Pop());
		#define BINARY_OP(op) { Value r = Pop(); Push(Pop() op r); }

		while (true) {
			#ifdef DEBUG
			Disassembler::DisassembleInstruction(m_Chunk, m_CodeIndex);
			#endif

			OpCode instruction = (OpCode)ReadByte();
			switch (instruction) {
				case OpCode::Constant:		Push(ReadConstant()); break;

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

				case OpCode::Return:
					Pop().PrintLn();
					return InterpretResult::OK;

				default:
					fprintf(stderr, "Unknown OpCode `%d'\n", instruction);
					return InterpretResult::RuntimeError;
			}
		}

		#undef UNARY_OP
		#undef BINARY_OP
	}

	uint16_t Runtime::ReadByte() {
		return m_Chunk->Code[m_CodeIndex++];
	}

	Value Runtime::ReadConstant() {
		uint16_t index = ReadByte();
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

	void Runtime::RuntimeError(const char* format, ...) {
		va_list args;
		va_start(args, format);
		vfprintf(stderr, format, args);
		va_end(args);
		fputs("\n", stderr);

		int line = m_Chunk->Lines[m_CodeIndex];
		fprintf(stderr, "[line %d] in script\n", line);
	}

}

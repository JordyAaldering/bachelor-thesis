#include "Runtime.h"
#include "Compiler.h"

#ifdef DEBUG
#include "Debug/Disassembler.h"
#endif

namespace Lang {

	std::shared_ptr<Chunk> Runtime::m_Chunk;
	uint16_t Runtime::m_CodeIndex;
	std::list<std::pair<std::string, Value>> Runtime::m_Variables;
	std::stack<Value> Runtime::m_Stack;

	InterpretResult Runtime::Interpret(const char* source) {
		m_Chunk = std::make_shared<Chunk>();
		if (!Compiler::Compile(source, m_Chunk)) {
			return InterpretResult::CompileError;
		}

		m_CodeIndex = 0;
		m_Stack = std::stack<Value>();
		InterpretResult res = Run();
		return res;
	}

	InterpretResult Runtime::Run() {
		#define UNARY_OP(op) Push(op Pop())
		#define BINARY_OP(op) { Value r = Pop(); Push(Pop() op r); }

		while (true) {
			#ifdef DEBUG
			Disassembler::DisassembleInstruction(m_Chunk, m_CodeIndex);
			#endif

			OpCode instruction = (OpCode)ReadByte();
			switch (instruction) {
			case OpCode::Constant:
				Push(ReadConstant());
				break;
			case OpCode::SetVariable: {
				std::string name = ReadVariable();
				Value value = m_Stack.top();
				m_Variables.emplace_front(name, value);
				break;
			}
			case OpCode::GetVariable: {
				std::string name = ReadVariable();
				Push(FindVariable(name));
				break;
			}
			case OpCode::PopVariable:
				m_Variables.erase(m_Variables.begin());
				break;

			case OpCode::Dim:
				Push(Pop().DimExpr());
				break;
			case OpCode::Shape:
				Push(Pop().ShapeExpr());
				break;
			case OpCode::Sel:
				Push(Pop().SelExpr(Pop()));
				break;

			case OpCode::Jump: {
				uint16_t offset = ReadShort();
				m_CodeIndex += offset;
				break;
			}
			case OpCode::JumpIfFalse: {
				uint16_t offset = ReadShort();
				if (!(bool)m_Stack.top()) m_CodeIndex += offset;
				break;
			}

			case OpCode::Not:			UNARY_OP(!);   break;
			case OpCode::Equal:			BINARY_OP(==); break;
			case OpCode::NotEqual:		BINARY_OP(!=); break;
			case OpCode::Greater:		BINARY_OP(>);  break;
			case OpCode::GreaterEqual:	BINARY_OP(>=); break;
			case OpCode::Less:			BINARY_OP(<);  break;
			case OpCode::LessEqual:		BINARY_OP(<=); break;

			case OpCode::Negate:		UNARY_OP(-);  break;
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

	uint8_t Runtime::ReadByte() {
		return m_Chunk->Code[m_CodeIndex++];
	}

	uint16_t Runtime::ReadShort() {
		uint8_t hi = m_Chunk->Code[m_CodeIndex] << 8;
		uint8_t lo = m_Chunk->Code[m_CodeIndex + 1];
		m_CodeIndex += 2;
		return (uint16_t)(hi | lo);
	}

	Value Runtime::ReadConstant() {
		uint8_t index = ReadByte();
		return m_Chunk->Constants[index];
	}

	std::string Runtime::ReadVariable() {
		uint8_t index = ReadByte();
		return m_Chunk->Variables[index];
	}

	Value Runtime::FindVariable(std::string name) {
		for (auto var : m_Variables) {
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

		int line = m_Chunk->Lines[m_CodeIndex];
		fprintf(stderr, "[line %d] in script\n", line);
	}

}

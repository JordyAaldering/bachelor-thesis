#pragma once

#include "Base.h"

namespace Lang {

	enum class OpCode {
		Return,
	};

	struct Chunk {
		std::vector<OpCode> Code;

		void Write(OpCode byte) {
			Code.push_back(byte);
		}
	};

}

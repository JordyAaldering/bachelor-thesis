#pragma once

#include "Base.h"
#include "Chunk.h"

namespace Lang {

	struct Function {
		const char* name;
		Chunk chunk;
		int arity;
		Function* next;

		Function();

		void Print();
	};

}

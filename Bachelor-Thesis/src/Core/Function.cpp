#include "Function.h"

namespace Lang {

	Function::Function()
		: name(nullptr), chunk(), arity(0), next(nullptr) {
	}

	void Function::Print() {
		if (name == nullptr) printf("<script>\n");
		else printf("<function %s>\n", name);
	}

}

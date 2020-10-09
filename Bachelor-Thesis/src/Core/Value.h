#pragma once

#include "Base.h"

namespace Lang {

	struct Value {
		uint8_t Dim;
		std::vector<uint8_t> Shape;
		std::vector<double> Values;

		Value(uint8_t dim, std::vector<uint8_t> shape, std::vector<double> values);

		operator bool() const;
		Value operator!();

		Value operator-();
		Value operator+(Value r);
		Value operator-(Value r);
		Value operator*(Value r);
		Value operator/(Value r);

		void Print();
	};

}

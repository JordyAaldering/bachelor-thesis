#pragma once

#include "Base.h"

namespace Lang {

	struct Value {
		uint16_t Dim;
		std::vector<uint16_t> Shape;
		std::vector<double> Values;

		Value(double value);
		Value(uint16_t dim, std::vector<uint16_t> shape, std::vector<double> values);

		Value operator!();
		Value operator!=(Value other);
		Value operator==(Value other);
		Value operator>(Value other);
		Value operator>=(Value other);
		Value operator<(Value other);
		Value operator<=(Value other);

		Value operator-();
		Value operator+(Value other);
		Value operator-(Value other);
		Value operator*(Value other);
		Value operator/(Value other);

		void Print();
		void PrintLn();
	};

}

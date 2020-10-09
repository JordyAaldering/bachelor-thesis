#pragma once

#include "Base.h"

namespace Lang {

	struct Value {
		uint8_t Dim;
		std::vector<uint8_t> Shape;
		std::vector<double> Values;

		Value(double value);
		Value(uint8_t dim, std::vector<uint8_t> shape, std::vector<double> values);

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

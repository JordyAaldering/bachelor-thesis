#include "Value.h"

namespace Lang {

	Value::Value(double value)
		: Dim(0), Shape({}), Values({ value }) {}

	Value::Value(uint8_t dim, std::vector<uint8_t> shape, std::vector<double> values)
		: Dim(dim), Shape(shape), Values(values) {}

#define EQUALITY_OP(op) \
	Value Value::operator op(Value other) { \
		for (int i = 0; i < Values.size(); i++) { \
			if (!(Values[i] op other.Values[i])) return 0; \
		} \
		return 1; \
	}

	Value Value::operator!() {
		for (double v : Values) {
			if (v != 0) return 0;
		}
		return 1;
	}

	EQUALITY_OP(!=)
	EQUALITY_OP(==)
	EQUALITY_OP(>)
	EQUALITY_OP(>=)
	EQUALITY_OP(<)
	EQUALITY_OP(<=)
#undef EQUALITY_OP

#define ARITH_OP(op) \
	Value Value::operator op(Value other) { \
		std::vector<double> v; \
		v.reserve(Values.size()); \
		for (int i = 0; i < Values.size(); i++) { \
			v.push_back(Values[i] op other.Values[i]); \
		} \
		return Value(Dim, Shape, v); \
	}

	Value Value::operator-() {
		std::vector<double> neg;
		neg.reserve(Values.size());
		for (double v : Values) {
			neg.push_back(-v);
		}
		return Value(Dim, Shape, neg);
	}

	ARITH_OP(+)
	ARITH_OP(-)
	ARITH_OP(*)
	ARITH_OP(/)
#undef ARITH_OP

	void Value::Print() {
		if (Dim == 0) {
			printf("<0, [], %f>", Values[0]);
			return;
		}

		printf("<%d, [%d", Dim, Shape[0]);
		int size = Shape[0];
		for (int i = 1; i < Dim; i++) {
			printf(", %d", Shape[i]);
			size *= Shape[i];
		}
		printf("], [%f", Values[0]);
		for (int i = 1; i < size; i++) {
			printf(", %f", Values[i]);
		}
		printf("]>");
	}

	void Value::PrintLn() {
		Print();
		printf("\n");
	}

}

#include "Value.h"

namespace Lang {

	Value::Value(double value)
		: Dim(0), Shape({}), Values({ value }) {
	}

	Value::Value(uint8_t dim, std::vector<uint16_t> shape, std::vector<double> values)
		: Dim(dim), Shape(shape), Values(values) {
	}

#define EQUALITY_OP(op) \
	Value Value::operator op(Value other) { \
		for (int i = 0; i < Values.size(); i++) \
			if (!(Values[i] op other.Values[i])) \
				return 0; \
		return 1; \
	}

	Value Value::operator!() {
		for (double v : Values)
			if (v != 0) return 0;
		return 1;
	}

	EQUALITY_OP(==)
	EQUALITY_OP(!=)
	EQUALITY_OP(>)
	EQUALITY_OP(>=)
	EQUALITY_OP(<)
	EQUALITY_OP(<=)
#undef EQUALITY_OP

#define ARITH_OP(op) \
	Value Value::operator op(Value other) { \
		std::vector<double> v; \
		v.reserve(Values.size()); \
		for (int i = 0; i < Values.size(); i++) \
			v.push_back(Values[i] op other.Values[i]); \
		return Value(Dim, Shape, v); \
	}

	Value Value::operator-() {
		std::vector<double> v;
		v.reserve(Values.size());
		for (double x : Values)
			v.push_back(-x);
		return Value(Dim, Shape, v);
	}

	ARITH_OP(+)
	ARITH_OP(-)
	ARITH_OP(*)
	ARITH_OP(/)
#undef ARITH_OP

	void Value::Print() {
		if (Dim == 0) {
			printf("<0, [], %f>\n", Values[0]);
			return;
		}

		printf("<%d, [", Dim);
		for (uint16_t s : Shape) {
			printf("%d,", s);
		}
		printf("], [");
		for (double v : Values) {
			printf("%f,", v);
		}
		printf("]>\n");
	}

}

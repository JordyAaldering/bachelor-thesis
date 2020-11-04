#include "Value.h"

namespace Lang {

	Value::Value(double value)
		: Dim(0), Shape({}), Values({ value }) {
	}

	Value::Value(uint8_t dim, std::vector<uint16_t> shape, std::vector<double> values)
		: Dim(dim), Shape(shape), Values(values) {
	}

	Value Value::DimExpr() {
		return { 0, {}, {(double)Dim} };
	}

	Value Value::ShapeExpr() {
		if (Dim == 0) {
			return { 0, {}, {0} };
		}

		std::vector<double> shape;
		shape.reserve(Shape.size());
		for (uint16_t s : Shape) {
			shape.push_back((double)s);
		}

		return { 1, {Dim}, shape };
	}

	Value Value::SelExpr(Value pos) {
		uint32_t index = 0;
		for (int i = 0; i < Dim; i++) {
			uint32_t offset = 1;
			for (int j = i + 1; j < Dim; j++) {
				offset *= (uint32_t)Shape[j];
			}

			index += offset * (uint32_t)pos.Values[i];
		}

		return { 0, {}, {Values[index]} };
	}

	Value::operator bool() const {
		for (double v : Values)
			if (v != 0) return true;
		return false;
	}

#define EQUALITY_OP(op) \
	Value Value::operator op(Value other) { \
		for (int i = 0; i < Values.size(); i++) \
			if (!(Values[i] op other.Values[i])) \
				return 0; \
		return 1; \
	}

	Value Value::operator!() {
		return !((bool)this);
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

		printf("<%d, [%d", Dim, Shape[0]);
		for (int i = 1; i < Dim; i++) {
			printf(", %d", Shape[i]);
		}
		printf("], [%f", Values[0]);
		for (int i = 1; i < Values.size(); i++) {
			printf(", %f", Values[i]);
		}
		printf("]>\n");
	}

}

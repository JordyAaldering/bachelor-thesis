#pragma once

#include "Base.h"

namespace Lang {

	struct Value {
		uint8_t Dim;
		std::vector<uint8_t> Shape;
		std::vector<double> Values;

		Value(uint8_t dim, std::vector<uint8_t> shape, std::vector<double> values)
			: Dim(dim), Shape(shape), Values(values) {
		}

		void Print() {
			if (Dim == 0) {
				printf("0, [], %f", Values[0]);
				return;
			}

			printf("%d, [%d", Dim, Shape[0]);
			int size = Shape[0];
			for (int i = 1; i < Dim; i++) {
				printf(", %d", Shape[i]);
				size *= Shape[i];
			}

			printf("], [%f", Values[0]);
			for (int i = 1; i < size; i++) {
				printf(", %f", Values[i]);
			}

			printf("]");
		}
	};

}

#pragma once

#include "Base.h"

namespace Lang {

	enum class TokenType {
		LeftParen, RightParen,
		LeftSquare, RightSquare, Comma,
		Plus, Minus, Star, Slash,
		Equal, EqualEqual,
		Bang, BangEqual,
		Greater, GreaterEqual,
		Less, LessEqual,
		And, Or,
		Number, Identifier,
		Function, Dim, Shape, Sel,
		Let, In, If, Then, Else,
		Error, Eof
	};

	static const char* TokenTypeMap[] = {
		"LeftParen", "RightParen",
		"LeftSquare", "RightSquare", "Comma",
		"Plus", "Minus", "Star", "Slash",
		"Equal", "EqualEqual",
		"Bang", "BangEqual",
		"Greater", "GreaterEqual",
		"Less", "LessEqual",
		"And", "Or",
		"Number", "Identifier",
		"Function", "Dim", "Shape", "Sel",
		"Let", "In", "If", "Then", "Else",
		"Error", "Eof"
	};

	static const char* ToString(TokenType type) {
		return TokenTypeMap[(uint8_t)type];
	}

}

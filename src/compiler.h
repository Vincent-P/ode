#pragma once
#include <stdint.h>
#include <string.h>

#define ARRAY_LENGTH(x) sizeof(x) / sizeof(x[0])

inline bool string_equals(const char *string1, uint64_t length1, const char *string2, uint64_t length2)
{
	if (length1 != length2) {
		return false;
	}

	const char *end1 = string1 + length1;
	for (; string1 < end1;) {
		if (*string1 != *string2) {
			return false;
		}
		string1 += 1;
		string2 += 1;
	}

	return true;
}

enum struct Result : uint64_t
{
	Ok,
	LexerDone,
	LexerUnknownToken,
	UnexpectedToken,
	ExpectedTokenGotEof,
	CompilerExpectedIdentifier,
	CompilerUnexpectedIdentifier,
	CompilerUnknownSymbol,
	Count,
};

inline const char *Result_str[] = {
	"Ok",
	"LexerDone",
	"LexerUnknownToken",
	"UnexpectedToken",
	"ExpectedTokenGotEof",
	"CompilerExpectedIdentifier",
	"CompilerUnexpectedIdentifier",
	"CompilerUnknownSymbol",
};
static_assert(ARRAY_LENGTH(Result_str) == uint64_t(Result::Count));

Result eval_content(const char *module_name, uint64_t module_name_length, const char *input, uint64_t input_length);

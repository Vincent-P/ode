#pragma once
#include "core.h"
#include "image.h"

enum struct Result : uint64_t
{
	Ok,
	LexerDone,
	LexerUnknownToken,
	UnexpectedToken,
	ExpectedTokenGotEof,
	CompilerExpectedStruct,
	CompilerExpectedIdentifier,
	CompilerExpectedExpr,
	CompilerUnexpectedIdentifier,
	CompilerUnexpectedExpression,
	CompilerUnknownSymbol,
	CompilerUnknownField,
	CompilerDuplicateSymbol,
	CompilerTooManyArgs,
	CompilerExpectedTypeGot,
	Fatal,
	Count,
};

inline const char *Result_str[] = {
	"Ok",
	"LexerDone",
	"LexerUnknownToken",
	"UnexpectedToken",
	"ExpectedTokenGotEof",
	"CompilerExpectedStruct",
	"CompilerExpectedIdentifier",
	"CompilerExpectedExpr",
	"CompilerUnexpectedIdentifier",
	"CompilerUnexpectedExpression",
	"CompilerUnknownSymbol",
	"CompilerUnknownField",
	"CompilerDuplicateSymbol",
	"CompilerTooManyArgs",
	"CompilerExpectedTypeGot",
	"Fatal",
};
static_assert(ARRAY_LENGTH(Result_str) == uint64_t(Result::Count));

struct Compiler;
Compiler *compiler_init();
Result compile_module(Compiler *compiler, sv module_name, sv input, Module **out_module);

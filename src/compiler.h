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
	CompilerExpectedIdentifier,
	CompilerExpectedExpr,
	CompilerUnexpectedIdentifier,
	CompilerUnknownSymbol,
	CompilerDuplicateSymbol,
	CompilerTooManyArgs,
	Fatal,
	Count,
};

inline const char *Result_str[] = {
	"Ok",
	"LexerDone",
	"LexerUnknownToken",
	"UnexpectedToken",
	"ExpectedTokenGotEof",
	"CompilerExpectedIdentifier",
	"CompilerExpectedExpr",
	"CompilerUnexpectedIdentifier",
	"CompilerUnknownSymbol",
	"CompilerDuplicateSymbol",
	"CompilerTooManyArgs",
	"Fatal",
};
static_assert(ARRAY_LENGTH(Result_str) == uint64_t(Result::Count));

/*
compiler compiles code source into bytecode
it can also stich multiple bytecode into instructions bank(s) for the executor

compiler = {}
compile_module(&compiler, module, code)
compile_module(&compiler, module, code)
compile_module(&compiler, module, code)

image = {}
compile_cook(&compiler, &image)

executor = {}
executor_init_memory(&executor, 1MB)
executor_set_image(&executor, image)

main loop exemple
    if code has changed
        compile
        image
        executor_set_image(&executor, image)
    execute(&executer)
*/
struct Compiler;
Compiler *compiler_init();
Result compile_module(
	Compiler *compiler, const char *module_name, uint64_t module_name_length, const char *input, uint64_t input_length);
Result compiler_make_image(Compiler *compiler, Image *image);

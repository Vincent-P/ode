#pragma once
#include "type_id.h"
#include "lexer.h"

enum struct ErrorCode : uint32_t
{
	Ok,
	LexerDone,
	LexerUnknownToken,
	UnexpectedToken,
	ExpectedTokenGotEof,
	ExpectedStruct,
	ExpectedIdentifier,
	ExpectedString,
	ExpectedExpr,
	UnexpectedIdentifier,
	UnexpectedExpression,
	UnknownSymbol,
	UnknownField,
	DuplicateSymbol,
	TooManyArgs,
	ExpectedTypeGot,
	Fatal,
	Assert,
	Count,
};

inline const char* ErrorCode_str[] = {
	"Ok",
	"LexerDone",
	"LexerUnknownToken",
	"UnexpectedToken",
	"ExpectedTokenGotEof",
	"ExpectedStruct",
	"ExpectedIdentifier",
	"ExpectedString",
	"ExpectedExpr",
	"UnexpectedIdentifier",
	"UnexpectedExpression",
	"UnknownSymbol",
	"UnknownField",
	"DuplicateSymbol",
	"TooManyArgs",
	"ExpectedTypeGot",
	"Fatal",
	"Assert",
	"Count",
};

struct Error
{
	ErrorCode code;

	sv msg;
	sv file;
	int line;

	span span;
	Token got_token;
	TypeID expected_type;
	TypeID got_type;
	uint64_t ip;
};


inline void error_trigger(Error *error, ErrorCode code, sv condition_str, sv file, int line)
{
	// Don't hide errors
	if (error->code != ErrorCode::Ok) {
		return;
	}

	error->code = code;
	error->msg = condition_str;
	error->file = file;
	error->line = line;

	// __debugbreak();
}
	
#define INIT_ERROR(error_ptr, errcode) (error_ptr)->file = sv_from_null_terminated(__FILE__); (error_ptr)->line = __LINE__; (error_ptr)->code = errcode;

#define ERROR_ASSERT(error_ptr, condition, code)                                                                                  \
	if ((condition) == false) {                                                                                        \
	error_trigger(error_ptr, code,                                                                                  \
	sv_from_null_terminated(#condition),                                                                       \
	sv_from_null_terminated(__FILE__),                                                                         \
	__LINE__);                                                                                                 \
	}

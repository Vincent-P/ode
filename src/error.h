#pragma once
#include "lexer.h"
#include "type_id.h"

typedef enum ErrorCode
{
	ErrorCode_Ok,
	ErrorCode_LexerUnknownToken,
	ErrorCode_UnexpectedToken,
	ErrorCode_ExpectedTokenGotEof,
	ErrorCode_ExpectedStruct,
	ErrorCode_ExpectedIdentifier,
	ErrorCode_ExpectedString,
	ErrorCode_ExpectedExpr,
	ErrorCode_UnexpectedIdentifier,
	ErrorCode_UnexpectedExpression,
	ErrorCode_UnknownSymbol,
	ErrorCode_UnknownField,
	ErrorCode_DuplicateSymbol,
	ErrorCode_TooManyArgs,
	ErrorCode_ExpectedTypeGot,
	ErrorCode_ExpectedNumericType,
	ErrorCode_Fatal,
	ErrorCode_Assert,
	ErrorCode_ModuleNotFound,
	ErrorCode_FunctionNotFound,
	ErrorCode_Count,
} ErrorCode;

const char *ErrorCode_str[] = {
	"Ok",
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
	"ExpectedNumericType",
	"Fatal",
	"Assert",
	"ModuleNotFound",
	"FunctionNotFound",
	"Count",
};

typedef struct Error
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
	uint32_t i_function;
} Error;

inline void error_trigger(Error *error, ErrorCode code, sv condition_str, sv file, int line)
{
	// Don't hide errors
	if (error->code != ErrorCode_Ok) {
		return;
	}

	error->code = code;
	error->msg = condition_str;
	error->file = file;
	error->line = line;
}

#define INIT_ERROR(error_ptr, errcode)                                                                                 \
	do {                                                                                                               \
		(error_ptr)->file = sv_from_null_terminated(__FILE__);                                                         \
		(error_ptr)->line = __LINE__;                                                                                  \
		(error_ptr)->code = errcode;                                                                                   \
	} while (0);

#define ERROR_ASSERT(error_ptr, condition, code)                                                                       \
	if ((condition) == false) {                                                                                        \
		error_trigger(error_ptr,                                                                                       \
			code,                                                                                                      \
			sv_from_null_terminated(#condition),                                                                       \
			sv_from_null_terminated(__FILE__),                                                                         \
			__LINE__);                                                                                                 \
	}

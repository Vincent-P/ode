#pragma once
#include "core/core.h"

typedef struct CompilationUnit CompilationUnit;

enum TokenKind
{
	TokenKind_Invalid,
	TokenKind_LeftParen,
	TokenKind_RightParen,
	TokenKind_UnsignedNumber,
	TokenKind_SignedNumber,
	TokenKind_FloatingNumber,
	TokenKind_Identifier,
	TokenKind_StringLiteral,
	TokenKind_Count,
};
typedef enum TokenKind TokenKind;

const char *TokenKind_str[] = {
	"Invalid",
	"LeftParen",
	"RightParen",
	"UnsignedNumber",
	"SignedNumber",
	"FloatNumber",
	"Identifier",
	"StringLiteral",
};
_Static_assert(ARRAY_LENGTH(TokenKind_str) == (uint64_t)(TokenKind_Count));

struct Token
{
	TokenKind kind;
	span span;
	union
	{
		uint32_t u32;
		int32_t i32;
		float f32;
		StringId sid;
	} data;
};
typedef struct Token Token;


struct LexerResult
{
	Token *tokens;
	bool success;
	span error_span;
};
typedef struct LexerResult LexerResult;
static LexerResult lexer_scan(Arena *arena, StringPool *string_pool, sv input);

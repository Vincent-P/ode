#pragma once
#include "core.h"

typedef struct CompilationUnit CompilationUnit;

enum TokenKind
{
	TokenKind_Invalid,
	TokenKind_LeftParen,
	TokenKind_RightParen,
	TokenKind_UnsignedNumber,
	TokenKind_SignedNumber,
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
	"Identifier",
	"StringLiteral",
};
_Static_assert(ARRAY_LENGTH(TokenKind_str) == (uint64_t)(TokenKind_Count));

struct Token
{
	TokenKind kind;
	span span;
	uint32_t data;
};
typedef struct Token Token;

void lexer_scan(CompilationUnit *compunit);

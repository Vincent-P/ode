#pragma once
#include "core.h"

struct CompilationUnit;

enum struct TokenKind : uint8_t
{
	Invalid,
	LeftParen,
	RightParen,
	Number,
	Identifier,
	StringLiteral,
	Count,
};
inline const char *TokenKind_str[] = {
	"Invalid",
	"LeftParen",
	"RightParen",
	"Number",
	"Identifier",
	"StringLiteral",
};
static_assert(ARRAY_LENGTH(TokenKind_str) == uint64_t(TokenKind::Count));

struct Token
{
	TokenKind kind;
	span span;
};

void lexer_scan(CompilationUnit *compunit);

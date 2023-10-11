#pragma once
#include "core.h"

enum struct LexerResult : uint8_t
{
	Ok,
	LexerDone,
	LexerUnknownToken,
	UnexpectedToken,
	ExpectedTokenGotEof,
	Fatal,
	Count,
};

inline const char *LexerResult_str[] = {
	"Ok",
	"LexerDone",
	"LexerUnknownToken",
	"UnexpectedToken",
	"ExpectedTokenGotEof",
	"Fatal",
};
static_assert(ARRAY_LENGTH(LexerResult_str) == uint8_t(LexerResult::Count));

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

struct Lexer
{
	// lexing data
	vec<uint32_t> line_endings;
	vec<Token> tokens;
	// error handling
	LexerResult result;
	span error;
};

void lexer_scan(Lexer *lexer, sv input);

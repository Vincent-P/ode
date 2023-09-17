#pragma once
#include "core.h"
#include "lexer.h" // Just for TokenKind :|

enum struct ParserResult : uint64_t
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

inline const char *ParserResult_str[] = {
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
static_assert(ARRAY_LENGTH(ParserResult_str) == uint64_t(ParserResult::Count));

struct AstNode
{
	span span;
	// Not null if atom
	uint32_t atom_token_index;
	// Not null if s-expr
	uint32_t left_child_index;
	uint32_t right_sibling_index;
};

struct Parser
{
	// input
	sv input;
	vec<Token> tokens;
	// output
	vec<AstNode> nodes;
	// parsing data
	uint32_t i_current_token;
	// error handling
	ParserResult result;
	span error;
	TokenKind expected_token_kind;
};

// Invalid index used for both atom_token_index and node indices
static constexpr uint32_t INVALID_NODE_INDEX = ~uint32_t(0);

void print_ast(sv input, const vec<Token> *tokens, const vec<AstNode> *nodes, uint32_t root_index);
vec<AstNode> parse_module(Parser *parser);

// Helpers for AstNode
inline bool ast_has_right_sibling(const AstNode *node) { return node->right_sibling_index != INVALID_NODE_INDEX; }
inline bool ast_has_left_child(const AstNode *node) { return node->left_child_index != INVALID_NODE_INDEX; }
inline bool ast_is_valid(uint32_t node_index) { return node_index != INVALID_NODE_INDEX; }
inline bool ast_is_atom(const AstNode *node) { return node->atom_token_index != INVALID_NODE_INDEX; }
inline const AstNode *ast_get_node(const vec<AstNode> *ast, uint32_t node_index) { return vec_at(ast, node_index); }	
inline const AstNode *ast_get_left_child(const vec<AstNode> *ast, const AstNode* node) { return vec_at(ast, node->left_child_index); }
inline const AstNode *ast_get_right_sibling(const vec<AstNode> *ast, const AstNode* node) { return vec_at(ast, node->right_sibling_index); }
inline const Token *ast_get_token(const vec<Token> *tokens, const AstNode *node) { return vec_at(tokens, node->atom_token_index); }

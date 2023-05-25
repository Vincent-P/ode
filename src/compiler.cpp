#include "compiler.h"
#include <corecrt_wstdio.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

inline constexpr uint64_t MAX_ARGUMENTS = 8;

enum struct TokenKind : uint64_t
{
	Invalid,
	LeftParen,
	RightParen,
	Number,
	Identifier,
	Count,
};

const char *TokenKind_str[] = {
	"Invalid",
	"LeftParen",
	"RightParen",
	"Number",
	"Identifier",
};
static_assert(ARRAY_LENGTH(TokenKind_str) == uint64_t(TokenKind::Count));

struct Token
{
	TokenKind kind;
	uint64_t offset;
	uint64_t length;
};

struct LexerState
{
	const char *input;
	uint64_t input_length;
	uint64_t offset;
	Result result;
	uint64_t error_offset;
	uint64_t error_length;
};

struct AstNode
{
	// Not null if atom
	const Token *atom_token;
	// Not null if s-expr
	AstNode *left_child;
	AstNode *right_sibling;
};

struct ParserState
{
	const char *input;
	uint64_t input_length;
	const Token *tokens;
	uint64_t token_length;
	AstNode *ast_nodes;
	uint64_t ast_nodes_capacity;
	uint64_t ast_nodes_length;
	Result result;
	uint64_t error_offset;
	uint64_t error_length;
	uint64_t i_current_token;
	TokenKind expected_token_kind;
};

enum struct TypeKind : uint8_t
{
	Int,
	Bool,
	Float,
	Pointer,
	Struct,
	Count,
};
const char *TypeKind_str[] = {
	"i32",
	"bool",
	"f32",
	"ptr",
	"struct",
};
static_assert(ARRAY_LENGTH(TypeKind_str) == uint64_t(TypeKind::Count));

uint64_t TypeKind_size[] = {
	4,
	1,
	4,
	4,
	1,
};
static_assert(ARRAY_LENGTH(TypeKind_size) == uint64_t(TypeKind::Count));

struct Type;
struct StructType
{
	const char *name;
	uint64_t name_length;
	const char *field_names[8];
	uint64_t field_names_length[8];
	Type *field_types[8];
	uint64_t field_offsets[8];
	uint64_t field_count;
};

struct Type
{
	TypeKind kind;
	uint64_t size;
	union
	{
		Type *pointee;
		StructType structure;
	};
};

struct Function
{
	const char *name;
	uint64_t name_length;
	uint64_t address; // offset into the compiler bytecode
	uint64_t arg_types[MAX_ARGUMENTS];
	uint64_t return_type;
};

struct Module
{
	const char *name;
	uint64_t name_length;
	Function *functions;
	uint64_t functions_capacity;
	uint64_t functions_length;
	OpCode *bytecode;
	uint64_t bytecode_capacity;
	uint64_t bytecode_length;
};

struct Compiler
{
	Module *modules;
	uint64_t modules_length;
	uint64_t modules_capacity;
};

struct LexicalScope
{
	const char **variables_name;
	uint64_t *variables_name_length;
	uint64_t *variables_type;
	uint64_t variables_length;
};

struct CompilerState
{
	uint64_t i_current_module;

	const char *input;
	uint64_t input_length;

	const Token *tokens;
	uint64_t token_length;

	const AstNode *ast_nodes;
	uint64_t ast_nodes_length;

	Type *types;
	uint64_t types_capacity;
	uint64_t types_length;

	LexicalScope *scopes;
	uint64_t scopes_capacity;
	uint64_t scopes_length;

	Result result;
	uint64_t error_offset;
	uint64_t error_length;
	Token got_token;
	Type *expected_type;
	Type *got_type;
};

bool is_identifier_char(char c)
{
	bool is_lower = 'a' <= c && c <= 'z';
	bool is_upper = 'A' <= c && c <= 'Z';
	bool is_number = '0' <= c && c <= '9';
	return c == '_' || c == '+' || c == '-' || c == '/' || c == '*' || is_lower || is_upper || is_number;
}

// Reserve the identifier that start with a number for literals
bool is_identifier_first_char(char c)
{
	bool is_number = '0' <= c && c <= '9';
	return is_identifier_char(c) && !is_number;
}

bool is_whitespace(char c)
{
	return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}

void lexer_next_token(LexerState *lexer, Token *result)
{
	if (lexer->result != Result::Ok) {
		return;
	}

	const char *input_cursor = lexer->input + lexer->offset;
	const char *input_end = lexer->input + lexer->input_length;

	while (input_cursor < input_end && is_whitespace(*input_cursor)) {
		input_cursor += 1;
	}

	if (input_cursor >= input_end) {
		*result = {};
		lexer->result = Result::LexerDone;
		return;
	}

	const char *first_char_cursor = input_cursor;
	const char first_char = *first_char_cursor;

	if (first_char == '(') {
		input_cursor += 1;
		result->kind = TokenKind::LeftParen;
		result->offset = uint64_t(first_char_cursor - lexer->input);
		result->length = 1;
	} else if (first_char == ')') {
		input_cursor += 1;
		result->kind = TokenKind::RightParen;
		result->offset = uint64_t(first_char_cursor - lexer->input);
		result->length = 1;
	} else if ('0' <= first_char && first_char <= '9') {
		input_cursor += 1;
		while (input_cursor < input_end && '0' <= *input_cursor && *input_cursor <= '9') {
			input_cursor += 1;
		}
		result->kind = TokenKind::Number;
		result->offset = uint64_t(first_char_cursor - lexer->input);
		result->length = uint64_t(input_cursor - first_char_cursor);
	} else if (is_identifier_first_char(first_char)) {
		input_cursor += 1;
		while (input_cursor < input_end && is_identifier_char(*input_cursor)) {
			input_cursor += 1;
		}
		result->kind = TokenKind::Identifier;
		result->offset = uint64_t(first_char_cursor - lexer->input);
		result->length = uint64_t(input_cursor - first_char_cursor);
	} else {
		*result = {};
		lexer->result = Result::LexerUnknownToken;
		lexer->error_offset = uint64_t(first_char_cursor - lexer->input);
		lexer->error_length = 1;
	}

	lexer->offset = uint64_t(input_cursor - lexer->input);
}

Token parser_current_token(ParserState *parser)
{
	if (parser->i_current_token < parser->token_length) {
		return parser->tokens[parser->i_current_token];
	} else {
		parser->result = Result::ExpectedTokenGotEof;
		parser->error_offset = 0;
		parser->error_length = 0;
		if (parser->token_length > 0) {
			const Token *last_token = parser->tokens + parser->token_length - 1;
			parser->error_offset = last_token->offset + last_token->length;
			parser->error_length = parser->input_length - parser->error_offset;
		}
		return {};
	}
}

Token parser_expect_token(ParserState *parser, TokenKind expect_kind)
{
	if (parser->result != Result::Ok) {
		return {};
	}

	if (parser->i_current_token >= parser->token_length) {
		parser->result = Result::ExpectedTokenGotEof;
		parser->error_offset = 0;
		parser->error_length = 0;
		if (parser->token_length > 0) {
			const Token *last_token = parser->tokens + parser->token_length - 1;
			parser->error_offset = last_token->offset + last_token->length;
			parser->error_length = parser->input_length - parser->error_offset;
		}
		return {};
	}

	Token token = parser->tokens[parser->i_current_token];

	if (token.kind == expect_kind) {
		parser->i_current_token += 1;
		parser->result = Result::Ok;
	} else {
		parser->result = Result::UnexpectedToken;
		parser->error_offset = token.offset;
		parser->error_length = token.length;
		parser->expected_token_kind = expect_kind;
	}
	return token;
}

AstNode *parser_push_ast_node_atom(ParserState *parser, const Token *token)
{
	if (parser->ast_nodes_length + 1 >= parser->ast_nodes_capacity) {
		return nullptr;
	}

	AstNode *new_node = parser->ast_nodes + parser->ast_nodes_length;
	parser->ast_nodes_length += 1;

	*new_node = {};
	new_node->atom_token = token;
	return new_node;
}

AstNode *parser_push_ast_node_sexpr(ParserState *parser, AstNode *first_child)
{
	if (parser->ast_nodes_length + 1 >= parser->ast_nodes_capacity) {
		return nullptr;
	}

	AstNode *new_node = parser->ast_nodes + parser->ast_nodes_length;
	parser->ast_nodes_length += 1;

	*new_node = {};
	new_node->left_child = first_child;
	return new_node;
}

void ast_node_add_child(AstNode *node, AstNode *new_child)
{
	if (node->left_child == nullptr) {
		node->left_child = new_child;
	} else {
		AstNode *child_cursor = node->left_child;
		while (child_cursor->right_sibling != nullptr) {
			child_cursor = child_cursor->right_sibling;
		}
		child_cursor->right_sibling = new_child;
	}
}

void ast_node_add_sibling(AstNode *child, AstNode *new_sibling)
{
	child->right_sibling = new_sibling;
}

void print_indent(int indent)
{
	for (int i = 0; i < indent; ++i) {
		putchar(' ');
		putchar(' ');
	}
}

void print_ast_rec(const char *input, AstNode *node, int indent)
{
	if (node->left_child != nullptr) {
		putchar('(');
	}

	if (node->atom_token != nullptr) {
		Token token = *node->atom_token;
		fprintf(stdout, "%s[%.*s]", TokenKind_str[uint64_t(token.kind)], int(token.length), input + token.offset);
	}

	if (node->left_child != nullptr) {
		AstNode *child = node->left_child;
		print_ast_rec(input, child, indent);

		while (child->right_sibling != nullptr) {
			child = child->right_sibling;
			putchar('\n');
			print_indent(indent + 1);
			print_ast_rec(input, child, indent + 1);
		}

		putchar(')');
	}
}

void print_ast(const char *input, AstNode *root)
{
	if (root == nullptr || root->left_child == nullptr) {
		return;
	}

	AstNode *child = root->left_child;
	print_ast_rec(input, child, 0);
	while (child->right_sibling != nullptr) {
		putchar('\n');
		child = child->right_sibling;
		print_ast_rec(input, child, 0);
	}
	putchar('\n');
}

// number | identifier
AstNode *parse_atom(ParserState *parser)
{
	Token current_token = parser_current_token(parser);
	if (current_token.kind == TokenKind::Number) {
		AstNode *new_node = parser_push_ast_node_atom(parser, parser->tokens + parser->i_current_token);
		parser->i_current_token += 1;
		return new_node;
	} else if (current_token.kind == TokenKind::Identifier) {
		AstNode *new_node = parser_push_ast_node_atom(parser, parser->tokens + parser->i_current_token);
		parser->i_current_token += 1;
		return new_node;
	} else {
		parser->result = Result::UnexpectedToken;
		parser->error_offset = current_token.offset;
		parser->error_length = current_token.length;
		parser->expected_token_kind = TokenKind::Number;
		return nullptr;
	}
}

// sexpr | atom
AstNode *parse_expression(ParserState *parser);

// () | (identifier exp*)
AstNode *parse_s_expression(ParserState *parser)
{
	auto left_paren_token = parser_expect_token(parser, TokenKind::LeftParen);
	(void)(left_paren_token);

	Token peek_token = parser_current_token(parser);
	// 0  - Empty list
	if (peek_token.kind == TokenKind::RightParen) {
		parser->i_current_token += 1;
		return parser_push_ast_node_sexpr(parser, nullptr);
	}

	// 1  - First symbol is something to evaluate (function, variable or builtin)
	AstNode *first_expr_node = parse_expression(parser);
	AstNode *sexpr_node = parser_push_ast_node_sexpr(parser, first_expr_node);

	// 2+ - arguments to evaluate the first symbol
	peek_token = parser_current_token(parser);
	AstNode *current_child = first_expr_node;
	while (peek_token.kind != TokenKind::RightParen && peek_token.kind != TokenKind::Invalid &&
		   parser->result == Result::Ok) {
		AstNode *new_expr_node = parse_expression(parser);
		ast_node_add_sibling(current_child, new_expr_node);
		current_child = new_expr_node;

		peek_token = parser_current_token(parser);
	}

	auto right_paren_token = parser_expect_token(parser, TokenKind::RightParen);
	(void)(right_paren_token);

	return sexpr_node;
}

// sexpr | atom
AstNode *parse_expression(ParserState *parser)
{
	Token i_current_token = parser_current_token(parser);
	if (i_current_token.kind == TokenKind::LeftParen) {
		return parse_s_expression(parser);
	} else {
		return parse_atom(parser);
	}
}

// sexpr*
AstNode *parse_module(ParserState *parser)
{
	AstNode *root_node = parser_push_ast_node_sexpr(parser, nullptr);

	AstNode *current_child = nullptr;
	while (parser->i_current_token < parser->token_length) {
		AstNode *expr_node = parse_s_expression(parser);
		if (current_child == nullptr) {
			root_node->left_child = expr_node;
		} else {
			current_child->right_sibling = expr_node;
		}
		current_child = expr_node;
		if (parser->result != Result::Ok)
			break;
	}

	return root_node;
}

// compiler helpers
void compiler_push_opcode(Compiler *compiler, CompilerState *compstate, OpCode opcode)
{
	Module *current_module = compiler->modules + compstate->i_current_module;
	if (current_module->bytecode_length + 1 >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		return;
	}

	current_module->bytecode[current_module->bytecode_length] = opcode;
	current_module->bytecode_length += 1;
}

Type *compiler_type_new(CompilerState *compstate)
{
	if (compstate->types_length + 1 >= compstate->types_capacity) {
		compstate->result = Result::Fatal;
		return nullptr;
	}
	Type *new_type = compstate->types + compstate->types_length;
	compstate->types_length += 1;
	return new_type;
}

bool type_similar(Type *lhs, Type *rhs)
{
	if (lhs == nullptr || rhs == nullptr) {
		return false;
	}

	if (lhs->kind != rhs->kind) {
		return false;
	} else if (lhs->kind == TypeKind::Pointer) {
		return type_similar(lhs->pointee, rhs->pointee);
	} else if (lhs->kind == TypeKind::Struct) {
		bool same_layout = lhs->structure.field_count == rhs->structure.field_count;
		for (uint64_t i = 0; same_layout && i < lhs->structure.field_count; ++i) {
			same_layout = type_similar(lhs->structure.field_types[i], rhs->structure.field_types[i]);
		}
		return same_layout;
	} else {
		return true;
	}
}

static constexpr uint64_t SCOPE_MAX_VARIABLES = 16;

void compiler_push_scope(CompilerState *compstate)
{
	if (compstate->scopes_length >= compstate->scopes_capacity) {
		compstate->result = Result::Fatal;
		return;
	}

	LexicalScope *new_scope = compstate->scopes + compstate->scopes_length;
	compstate->scopes_length += 1;

	*new_scope = {};
	new_scope->variables_name = (const char **)calloc(SCOPE_MAX_VARIABLES, sizeof(const char *));
	new_scope->variables_name_length = (uint64_t *)calloc(SCOPE_MAX_VARIABLES, sizeof(uint64_t));
	new_scope->variables_type = (uint64_t *)calloc(SCOPE_MAX_VARIABLES, sizeof(uint64_t));
}

void compiler_pop_scope(CompilerState *compstate)
{
	if (compstate->scopes_length == 0) {
		compstate->result = Result::Fatal;
		return;
	}

	LexicalScope *current_scope = compstate->scopes + compstate->scopes_length - 1;
	compstate->scopes_length -= 1;
	free(current_scope->variables_name);
	free(current_scope->variables_name_length);
	free(current_scope->variables_type);
}

bool compiler_push_variable(
	CompilerState *compstate, const Token *identifier_token, uint64_t i_type, uint64_t *i_variable_out)
{
	if (compstate->scopes_length >= compstate->scopes_capacity) {
		compstate->result = Result::Fatal;
		return false;
	}

	LexicalScope *current_scope = compstate->scopes + compstate->scopes_length - 1;
	if (current_scope->variables_length >= SCOPE_MAX_VARIABLES) {
		compstate->result = Result::Fatal;
		return false;
	}

	uint64_t i_new_variable = current_scope->variables_length;
	current_scope->variables_length += 1;

	current_scope->variables_name[i_new_variable] = compstate->input + identifier_token->offset;
	current_scope->variables_name_length[i_new_variable] = identifier_token->length;
	current_scope->variables_type[i_new_variable] = i_type;

	*i_variable_out = i_new_variable;
	return true;
}

bool compiler_lookup_variable(
	CompilerState *compstate, const Token *identifier_token, Type **type_out, uint64_t *i_variable_out)
{
	if (compstate->scopes_length == 0) {
		compstate->result = Result::Fatal;
		return false;
	}

	const char *tofind_name = compstate->input + identifier_token->offset;
	uint64_t tofind_name_length = identifier_token->length;

	for (uint64_t i_scope = compstate->scopes_length - 1; i_scope < compstate->scopes_length; --i_scope) {
		LexicalScope *scope = compstate->scopes + i_scope;

		uint64_t i_found = 0;
		Type *found_type = nullptr;
		for (uint64_t i_variable = 0; i_variable < scope->variables_length; ++i_variable) {
			const char *variable_name = scope->variables_name[i_variable];
			uint64_t variable_name_length = scope->variables_name_length[i_variable];

			if (string_equals(tofind_name, tofind_name_length, variable_name, variable_name_length)) {
				i_found = i_variable;
				found_type = compstate->types + scope->variables_type[i_variable];
			}
		}

		if (found_type != nullptr) {
			*type_out = found_type;
			*i_variable_out = i_found;
			return true;
		}
	}

	return false;
}

// compiler
Type *compile_sexpr(Compiler *compiler, CompilerState *compstate, const AstNode *node);

Type *compile_atom(Compiler *compiler, CompilerState *compstate, const AstNode *expr_node)
{
	const Token token = *expr_node->atom_token;

	if (token.kind == TokenKind::Identifier) {
		// Refer a declared variable
		// <identifier>
		fprintf(stdout, "Expr evaluated to %.*s\n", int(token.length), compstate->input + token.offset);
		Type *ty = nullptr;
		uint64_t i_variable = 0;
		if (!compiler_lookup_variable(compstate, &token, &ty, &i_variable)) {
			compstate->result = Result::CompilerUnknownSymbol;
			compstate->error_offset = token.offset;
			compstate->error_length = token.length;
			return nullptr;
		}
		compiler_push_opcode(compiler, compstate, OpCode::GetLocal);
		return ty;
	} else if (token.kind == TokenKind::Number) {
		// An integer constant
		// <number>
		int token_number = int_from_string(compstate->input + token.offset, token.length);
		fprintf(stdout, "Expr evaluated to %d\n", token_number);
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Int;
		compiler_push_opcode(compiler, compstate, OpCode::Constant);
		return ty;
	} else {
		compstate->result = Result::Fatal;
		return nullptr;
	}
}

// <identifier> | <number> | <s-expression>
Type *compile_expr(Compiler *compiler, CompilerState *compstate, const AstNode *expr_node)
{
	if (compstate->result != Result::Ok) {
		return nullptr;
	}

	if (expr_node->atom_token != nullptr) {
		return compile_atom(compiler, compstate, expr_node);
	} else if (expr_node->left_child != nullptr) {
		return compile_sexpr(compiler, compstate, expr_node);
	} else {
		compstate->result = Result::Fatal;
		return nullptr;
	}
}

// Defines a new function
// (define <name> <return_type> <body expression>)
Type *compile_define(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *define_token_node = node->left_child;
	const AstNode *name_node = define_token_node->right_sibling;

	if (name_node == nullptr || name_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return nullptr;
	}

	const AstNode *return_type_node = name_node->right_sibling;
	if (return_type_node == nullptr || return_type_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return nullptr;
	}

	const AstNode *body_node = return_type_node->right_sibling;
	if (body_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return nullptr;
	}

	if (body_node->right_sibling != nullptr) {
		compstate->result = Result::CompilerUnexpectedExpression;
		return nullptr;
	}

	const Token name_token = *name_node->atom_token;
	const char *name_token_str = compstate->input + name_token.offset;
	const Token return_token = *return_type_node->atom_token;

	// -- Type checking
	Module *current_module = compiler->modules + compstate->i_current_module;
	fprintf(stdout,
		"Defining new function: %.*s -> %.*s\n",
		int(name_token.length),
		compstate->input + name_token.offset,
		int(return_token.length),
		compstate->input + return_token.offset);
	for (uint64_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (string_equals(function->name, function->name_length, name_token_str, name_token.length)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			// Actually it should be possible to recompile a function if signature has not changed.
			return nullptr;
		}
	}

	if (current_module->functions_length + 1 >= current_module->functions_capacity) {
		compstate->result = Result::Fatal;
		return nullptr;
	}

	// Create a new function symbol
	Function *function = current_module->functions + current_module->functions_length;
	function->name = name_token_str;
	function->name_length = name_token.length;
	function->address = current_module->bytecode_length;
	current_module->functions_length += 1;

	// -- Compiling
	compiler_push_scope(compstate);
	// TODO arguments
	compile_expr(compiler, compstate, body_node);
	compiler_push_opcode(compiler, compstate, OpCode::Ret);
	compiler_pop_scope(compstate);
	return nullptr;
}

Type *compile_let(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *let_token_node = node->left_child;
	const AstNode *name_node = let_token_node->right_sibling;

	if (name_node == nullptr || name_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return nullptr;
	}

	const AstNode *value_node = name_node->right_sibling;
	if (value_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return nullptr;
	}

	const Token name_token = *name_node->atom_token;

	// -- Type checking
	fprintf(stdout, "Defining new local: %.*s\n", int(name_token.length), compstate->input + name_token.offset);

	// Compile the body
	Type *expr_type = compile_expr(compiler, compstate, value_node);
	if (expr_type == nullptr) {
		return nullptr;
	}

	uint64_t i_expr_type = uint64_t(expr_type - compstate->types);
	if (i_expr_type >= compstate->types_length) {
		compstate->result = Result::Fatal;
		return nullptr;
	}
	uint64_t i_variable = 0;
	if (!compiler_push_variable(compstate, &name_token, i_expr_type, &i_variable)) {
		return nullptr;
	}

	compiler_push_opcode(compiler, compstate, OpCode::SetLocal);

	return expr_type;
}

Type *compile_begin(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *begin_node = node->left_child;
	if (begin_node == nullptr || begin_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return nullptr;
	}

	const AstNode *current_expr_node = begin_node->right_sibling;
	if (current_expr_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return nullptr;
	}

	Type *return_type = nullptr;
	while (current_expr_node != nullptr) {
		return_type = compile_expr(compiler, compstate, current_expr_node);
		current_expr_node = current_expr_node->right_sibling;
	}

	return return_type;
}

// Adds two integers
// (+ <lhs> <rhs>)
Type *compile_iadd(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *plus_token_node = node->left_child;

	const AstNode *lhs_node = plus_token_node->right_sibling;
	if (lhs_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return nullptr;
	}

	const AstNode *rhs_node = lhs_node->right_sibling;
	if (rhs_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return nullptr;
	}

	if (rhs_node->right_sibling != nullptr) {
		compstate->result = Result::CompilerTooManyArgs;
		return nullptr;
	}

	// -- Type checking
	Type *lhs = compile_expr(compiler, compstate, lhs_node);
	Type *rhs = compile_expr(compiler, compstate, rhs_node);
	if (compstate->result != Result::Ok) {
		return nullptr;
	}

	Type int_type = {};
	int_type.kind = TypeKind::Int;
	bool lhs_valid = type_similar(lhs, &int_type);
	bool rhs_valid = type_similar(rhs, &int_type);
	bool valid_types = lhs_valid && rhs_valid;
	if (!valid_types) {
		Type *int_type_stored = compiler_type_new(compstate);
		*int_type_stored = int_type;

		compstate->result = Result::CompilerExpectedTypeGot;
		compstate->expected_type = int_type_stored;
		compstate->got_type = lhs_valid ? rhs : lhs;
	}

	// -- Compiling
	compiler_push_opcode(compiler, compstate, OpCode::IAdd);

	return lhs;
}

using CompstateBuiltin = Type *(*)(Compiler *, CompilerState *, const AstNode *);
CompstateBuiltin compiler_builtins[] = {
	compile_define,
	compile_let,
	compile_begin,
	compile_iadd,
};
const char *compstate_builtins_str[] = {
	"define",
	"let",
	"begin",
	"+",
};
static_assert(ARRAY_LENGTH(Result_str) == uint64_t(Result::Count));

// (<identifier> <expr>*)
Type *compile_sexpr(Compiler *compiler, CompilerState *compstate, const AstNode *function_node)
{
	// -- Parsing
	const AstNode *identifier_node = function_node->left_child;
	if (identifier_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return nullptr;
	}

	const Token identifier = *identifier_node->atom_token;
	const char *identifier_string = compstate->input + identifier.offset;

	// Dispatch to the correct builtin compile function (define, iadd, etc)
	const uint64_t compstate_builtin_length = ARRAY_LENGTH(compiler_builtins);
	for (uint64_t i = 0; i < compstate_builtin_length; ++i) {
		if (string_equals(identifier_string,
				identifier.length,
				compstate_builtins_str[i],
				strlen(compstate_builtins_str[i]))) {
			return compiler_builtins[i](compiler, compstate, function_node);
		}
	}

	// If the identifier is not a builtin, generate a function call
	// -- Type checking / Compiling
	fprintf(stdout, "Calling function '%.*s'\n", int(identifier.length), identifier_string);
	compstate->result = Result::CompilerUnknownSymbol;
	compstate->error_offset = identifier.offset;
	compstate->error_length = identifier.length;

	const AstNode *arg_node = identifier_node->right_sibling;
	while (arg_node != nullptr) {
		compile_expr(compiler, compstate, arg_node);
		arg_node = arg_node->right_sibling;
	}

	return nullptr;
}

// A module is the "root" of a script, a series of S-expression
void compile_module(Compiler *compiler, CompilerState *compstate)
{
	const AstNode *root_node = compstate->ast_nodes;

	const AstNode *root_expr = root_node->left_child;
	while (root_expr != nullptr) {

		const AstNode *first_sexpr_member = root_expr->left_child;

		const bool not_an_atom = first_sexpr_member == nullptr || first_sexpr_member->atom_token == nullptr;
		const bool not_an_identifier = not_an_atom || first_sexpr_member->atom_token->kind != TokenKind::Identifier;
		if (not_an_identifier) {
			compstate->result = Result::CompilerExpectedIdentifier;
			return;
		}

		compile_sexpr(compiler, compstate, root_expr);
		if (compstate->result != Result::Ok) {
			return;
		}

		root_expr = root_expr->right_sibling;
	}
}

Compiler *compiler_init()
{
	Compiler *compiler = static_cast<Compiler *>(calloc(1, sizeof(Compiler)));
	compiler->modules_capacity = 8;
	compiler->modules = static_cast<Module *>(calloc(compiler->modules_capacity, sizeof(Module)));
	return compiler;
};

void module_init(Module *new_module)
{
	const uint64_t functions_capacity = 8;
	new_module->functions_capacity = functions_capacity;
	new_module->functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));

	const uint64_t bytecode_capacity = 1024;
	new_module->bytecode_capacity = bytecode_capacity;
	new_module->bytecode = static_cast<OpCode *>(calloc(bytecode_capacity, sizeof(OpCode)));
}

Result compile_module(
	Compiler *compiler, const char *module_name, uint64_t module_name_length, const char *input, uint64_t input_length)
{
	LexerState lexer = {};
	lexer.input = input;
	lexer.input_length = input_length;

	uint64_t tokens_capacity = 4096;
	Token *tokens = (Token *)malloc(tokens_capacity * sizeof(Token));
	uint64_t tokens_length = 0;

	while (tokens_length < tokens_capacity) {
		lexer_next_token(&lexer, &tokens[tokens_length]);
		if (lexer.result == Result::LexerDone) {
			break;
		}
		tokens_length += 1;
	}

	if (lexer.result != Result::LexerDone) {
		fprintf(stderr,
			"# Lexer[input_length: %zu, offset: %zu] returned %s\n",
			lexer.input_length,
			lexer.offset,
			Result_str[uint64_t(lexer.result)]);

		fprintf(stderr,
			"Error at (%zu, %zu): '%.*s'\n",
			lexer.error_offset,
			lexer.error_length,
			int(lexer.error_length),
			lexer.input + lexer.error_offset);

		if (lexer.offset < lexer.input_length) {
			fprintf(stderr, "# Stopped at char '%c'\n", lexer.input[lexer.offset]);
		}
		return lexer.result;
	}

	uint64_t ast_nodes_capacity = 4096;
	AstNode *ast_nodes = (AstNode *)malloc(ast_nodes_capacity * sizeof(AstNode));

	ParserState parser = {};
	parser.input = lexer.input;
	parser.input_length = lexer.input_length;
	parser.tokens = tokens;
	parser.token_length = tokens_length;
	parser.ast_nodes = ast_nodes;
	parser.ast_nodes_capacity = ast_nodes_capacity;
	parser.ast_nodes_length = 0;
	parse_module(&parser);

	print_ast(lexer.input, parser.ast_nodes);

	if (parser.result != Result::Ok) {
		fprintf(stderr,
			"# Parser[token_length: %zu, i_current_token: %zu] returned %s\n",
			parser.token_length,
			parser.i_current_token,
			Result_str[uint64_t(parser.result)]);

		fprintf(stderr,
			"Error at (%zu, %zu): '%.*s'\n",
			parser.error_offset,
			parser.error_length,
			int(parser.error_length),
			parser.input + parser.error_offset);

		if (parser.token_length > 0) {
			uint64_t i_last_token =
				parser.i_current_token < parser.token_length ? parser.i_current_token : parser.token_length - 1;
			Token last_token = tokens[i_last_token];
			const char *token_kind_str = TokenKind_str[uint64_t(last_token.kind)];
			fprintf(stderr,
				"# Last seen token is %s[%.*s]\n",
				token_kind_str,
				int(last_token.length),
				lexer.input + last_token.offset);
		}

		if (parser.expected_token_kind != TokenKind::Invalid) {
			fprintf(stderr, "# Expected token of kind %s\n", TokenKind_str[uint64_t(parser.expected_token_kind)]);
		}

		return parser.result;
	}

	// Find module
	uint64_t i_module = 0;
	for (; i_module < compiler->modules_length; ++i_module) {
		Module *module = compiler->modules + i_module;
		if (string_equals(module->name, module->name_length, module_name, module_name_length)) {
			break;
		}
	}
	// Not found, create a new module
	if (i_module >= compiler->modules_length) {
		if (i_module >= compiler->modules_capacity) {
			return Result::Fatal;
		}

		i_module = compiler->modules_length;
		Module *new_module = compiler->modules + i_module;
		module_init(new_module);
		compiler->modules_length += 1;
	}

	LexicalScope scopes[16] = {};

	CompilerState compstate = {};
	compstate.i_current_module = i_module;
	compstate.input = parser.input;
	compstate.input_length = parser.input_length;
	compstate.tokens = parser.tokens;
	compstate.token_length = parser.token_length;
	compstate.ast_nodes = parser.ast_nodes;
	compstate.ast_nodes_length = parser.ast_nodes_length;
	compstate.types_capacity = 64;
	compstate.types = (Type *)calloc(compstate.types_capacity, sizeof(Type));
	compstate.scopes = scopes;
	compstate.scopes_capacity = 16;
	compile_module(compiler, &compstate);

	if (compstate.result != Result::Ok) {
		fprintf(stderr, "# Compstate[] returned %s\n", Result_str[uint64_t(compstate.result)]);
		fprintf(stderr,
			"Error at (%zu, %zu): '%.*s'\n",
			compstate.error_offset,
			compstate.error_length,
			int(compstate.error_length),
			compstate.input + compstate.error_offset);

		if (compstate.expected_type == nullptr) {
			fprintf(stderr, "# expected type %s\n", "<nullptr>");
		} else {
			fprintf(stderr, "# expected type %s\n", TypeKind_str[uint64_t(compstate.expected_type->kind)]);
		}

		if (compstate.got_type == nullptr) {
			fprintf(stderr, "# got type %s\n", "<nullptr>");
		} else {
			fprintf(stderr, "# got type %s\n", TypeKind_str[uint64_t(compstate.got_type->kind)]);
		}

		return compstate.result;
	}

	fprintf(stdout, "Compilation success:\n");
	Module *compiled_module = compiler->modules + compstate.i_current_module;
	for (uint64_t i_opcode = 0; i_opcode < compiled_module->bytecode_length; ++i_opcode) {
		OpCode opcode = compiled_module->bytecode[i_opcode];
		fprintf(stdout, "%s\n", OpCode_str[uint8_t(opcode)]);
	}

	return Result::Ok;
}

#include "compiler.h"
#include <corecrt_wstdio.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

inline constexpr uint64_t MAX_ARGUMENTS = 8;

struct TextInput
{
	sv text;
	uint64_t *line_endings;
	uint64_t line_endings_length;
};

void text_input_get_line_col(const TextInput *input, uint64_t at, uint64_t *line, uint64_t *col)
{
	uint64_t i_line = 0;
	uint64_t last_line_ending = 0;
	for (; i_line + 1 < input->line_endings_length; ++i_line) {
		if (input->line_endings[i_line] > at) {
			break;
		}
		last_line_ending = input->line_endings[i_line];
	}

	*line = i_line;
	*col = at - last_line_ending;
}

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
	span span;
};

struct LexerState
{
	TextInput input;
	uint64_t offset;
	Result result;
	span error;
};

struct AstNode
{
	span span;
	// Not null if atom
	const Token *atom_token;
	// Not null if s-expr
	AstNode *left_child;
	AstNode *right_sibling;
};

struct ParserState
{
	TextInput input;
	const Token *tokens;
	uint64_t token_length;
	AstNode *ast_nodes;
	uint64_t ast_nodes_capacity;
	uint64_t ast_nodes_length;
	Result result;
	span error;
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
	sv name;
	sv field_names[8];
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
	sv name;
	uint64_t address; // offset into the compiler bytecode
	uint64_t arg_types[MAX_ARGUMENTS];
	uint64_t return_type;
};

struct Module
{
	sv name;
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
	sv *variables_name;
	uint64_t *variables_type;
	uint64_t variables_length;
};

struct CompilerState
{
	uint64_t i_current_module;

	TextInput input;

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
	span error;
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

	uint64_t to_eat = 0;
	for (; to_eat < lexer->input.text.length; ++to_eat) {
		if (!is_whitespace(lexer->input.text.chars[to_eat])) {
			break;
		}
	}

	lexer->input.text = sv_offset(lexer->input.text, to_eat);
	lexer->offset += to_eat;

	if (lexer->input.text.length == 0) {
		*result = {};
		lexer->result = Result::LexerDone;
		return;
	}

	const char first_char = lexer->input.text.chars[0];
	uint64_t token_length = 1;

	result->span.start = lexer->offset;
	if (first_char == '(') {
		result->kind = TokenKind::LeftParen;
	} else if (first_char == ')') {
		result->kind = TokenKind::RightParen;
	} else if ('0' <= first_char && first_char <= '9') {
		while (token_length < lexer->input.text.length && '0' <= lexer->input.text.chars[token_length] &&
			   lexer->input.text.chars[token_length] <= '9') {
			token_length += 1;
		}
		result->kind = TokenKind::Number;
	} else if (is_identifier_first_char(first_char)) {
		while (token_length < lexer->input.text.length && is_identifier_char(lexer->input.text.chars[token_length])) {
			token_length += 1;
		}
		result->kind = TokenKind::Identifier;
	} else {
		*result = {};
		lexer->result = Result::LexerUnknownToken;
		lexer->error = span{lexer->offset, lexer->offset + 1};
	}
	result->span.end = lexer->offset + token_length;
	lexer->offset += token_length;
	lexer->input.text = sv_offset(lexer->input.text, token_length);
}

Token parser_current_token(ParserState *parser)
{
	if (parser->i_current_token < parser->token_length) {
		return parser->tokens[parser->i_current_token];
	} else {
		parser->result = Result::ExpectedTokenGotEof;
		parser->error = {};
		if (parser->token_length > 0) {
			const Token *last_token = parser->tokens + parser->token_length - 1;
			parser->error = last_token->span;
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
		parser->error = {};
		if (parser->token_length > 0) {
			const Token *last_token = parser->tokens + parser->token_length - 1;
			parser->error = last_token->span;
		}
		return {};
	}

	Token token = parser->tokens[parser->i_current_token];

	if (token.kind == expect_kind) {
		parser->i_current_token += 1;
		parser->result = Result::Ok;
	} else {
		parser->result = Result::UnexpectedToken;
		parser->error = token.span;
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

void print_ast_rec(sv input, AstNode *node, int indent)
{
	if (node->left_child != nullptr) {
		putchar('(');
	}

	if (node->atom_token != nullptr) {
		Token token = *node->atom_token;
		sv token_str = sv_substr(input, token.span);
		fprintf(stdout, "%s[%.*s]", TokenKind_str[uint64_t(token.kind)], int(token_str.length), token_str.chars);
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

void print_ast(sv input, AstNode *root)
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
		new_node->span = current_token.span;
		parser->i_current_token += 1;
		return new_node;
	} else if (current_token.kind == TokenKind::Identifier) {
		AstNode *new_node = parser_push_ast_node_atom(parser, parser->tokens + parser->i_current_token);
		new_node->span = current_token.span;
		parser->i_current_token += 1;
		return new_node;
	} else {
		parser->result = Result::UnexpectedToken;
		parser->error = current_token.span;
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

	sexpr_node->span = span_extend(left_paren_token.span, right_paren_token.span);

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
	new_scope->variables_name = (sv *)calloc(SCOPE_MAX_VARIABLES, sizeof(sv));
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

	current_scope->variables_name[i_new_variable] = sv_substr(compstate->input.text, identifier_token->span);
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

	sv tofind_name = sv_substr(compstate->input.text, identifier_token->span);

	for (uint64_t i_scope = compstate->scopes_length - 1; i_scope < compstate->scopes_length; --i_scope) {
		LexicalScope *scope = compstate->scopes + i_scope;

		uint64_t i_found = 0;
		Type *found_type = nullptr;
		for (uint64_t i_variable = 0; i_variable < scope->variables_length; ++i_variable) {
			sv variable_name = scope->variables_name[i_variable];
			if (sv_equals(tofind_name, variable_name)) {
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

Type *parse_type(Compiler *, CompilerState *compstate, const AstNode *node)
{
	if (node == nullptr || node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		compstate->error = node->span;
		return nullptr;
	}

	const Token identifier = *node->atom_token;
	sv identifier_str = sv_substr(compstate->input.text, identifier.span);

	if (sv_equals(identifier_str, sv_from_null_terminated(TypeKind_str[static_cast<uint32_t>(TypeKind::Int)]))) {
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Int;
		return ty;
	} else if (sv_equals(identifier_str,
				   sv_from_null_terminated(TypeKind_str[static_cast<uint32_t>(TypeKind::Bool)]))) {
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Bool;
		return ty;
	} else if (sv_equals(identifier_str,
				   sv_from_null_terminated(TypeKind_str[static_cast<uint32_t>(TypeKind::Float)]))) {
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Float;
		return ty;
	} else {
		compstate->result = Result::CompilerUnknownSymbol;
		compstate->error = node->span;
		return nullptr;
	}
}

Type *compile_atom(Compiler *compiler, CompilerState *compstate, const AstNode *expr_node)
{
	const Token token = *expr_node->atom_token;
	sv token_sv = sv_substr(compstate->input.text, token.span);

	if (token.kind == TokenKind::Identifier) {
		// Refer a declared variable
		// <identifier>
		fprintf(stdout, "Expr evaluated to %.*s\n", int(token_sv.length), token_sv.chars);
		Type *ty = nullptr;
		uint64_t i_variable = 0;
		if (!compiler_lookup_variable(compstate, &token, &ty, &i_variable)) {
			compstate->result = Result::CompilerUnknownSymbol;
			compstate->error = token.span;
			return nullptr;
		}
		compiler_push_opcode(compiler, compstate, OpCode::GetLocal);
		return ty;
	} else if (token.kind == TokenKind::Number) {
		// An integer constant
		// <number>
		int token_number = sv_to_int(token_sv);
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
	Token name_token = {};
	Token arg_identifiers[MAX_ARGUMENTS] = {};
	const AstNode *arg_nodes[MAX_ARGUMENTS] = {};
	uint64_t args_length = 0;

	// -- Parsing
	const AstNode *define_token_node = node->left_child;

	const AstNode *name_node = define_token_node->right_sibling;
	const AstNode *return_type_node = nullptr;
	{
		const AstNode *identifier_node = name_node->left_child;
		if (identifier_node == nullptr || identifier_node->atom_token == nullptr) {
			compstate->result = Result::CompilerExpectedIdentifier;
			compstate->error = node->span;
			return nullptr;
		}
		name_token = *identifier_node->atom_token;

		return_type_node = identifier_node->right_sibling;
		if (return_type_node == nullptr) {
			compstate->result = Result::CompilerExpectedExpr;
			compstate->error = node->span;
			return nullptr;
		}
	}

	const AstNode *arglist_node = name_node->right_sibling;
	{
		const AstNode *identifier_node = arglist_node->left_child;

		while (identifier_node != nullptr) {
			if (identifier_node == nullptr || identifier_node->atom_token == nullptr) {
				compstate->result = Result::CompilerExpectedIdentifier;
				compstate->error = arglist_node->span;
				return nullptr;
			}

			const AstNode *type_node = identifier_node->right_sibling;
			if (type_node == nullptr) {
				compstate->result = Result::CompilerExpectedExpr;
				compstate->error = arglist_node->span;
				return nullptr;
			}

			arg_identifiers[args_length] = *identifier_node->atom_token;
			arg_nodes[args_length] = type_node;
			args_length += 1;

			identifier_node = type_node->left_child;
		}
	}

	const AstNode *body_node = arglist_node->right_sibling;
	if (body_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		compstate->error = node->span;
		return nullptr;
	}

	if (body_node->right_sibling != nullptr) {
		compstate->result = Result::CompilerUnexpectedExpression;
		compstate->error = node->span;
		return nullptr;
	}

	sv name_token_str = sv_substr(compstate->input.text, name_token.span);

	// -- Type checking
	Module *current_module = compiler->modules + compstate->i_current_module;
	fprintf(stdout,
		"Defining new function: %.*s (arity %zu)\n",
		int(name_token_str.length),
		name_token_str.chars,
		args_length);

	for (uint64_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, name_token_str)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			compstate->error = node->span;
			// Actually it should be possible to recompile a function if signature has not changed.
			return nullptr;
		}
	}

	if (current_module->functions_length + 1 >= current_module->functions_capacity) {
		compstate->result = Result::Fatal;
		compstate->error = node->span;
		return nullptr;
	}

	// Create a new function symbol
	Function *function = current_module->functions + current_module->functions_length;
	function->name = name_token_str;
	function->address = current_module->bytecode_length;

	current_module->functions_length += 1;

	// -- Compiling
	Type *return_type = parse_type(compiler, compstate, return_type_node);
	if (return_type == nullptr) {
		return nullptr;
	}
	uint64_t i_return_type = uint64_t(return_type - compstate->types);
	if (i_return_type >= compstate->types_length) {
		compstate->result = Result::Fatal;
		compstate->error = node->span;
		return nullptr;
	}
	function->return_type = i_return_type;

	compiler_push_scope(compstate);

	for (uint64_t i_arg = 0; i_arg < args_length; ++i_arg) {
		Type *arg_type = parse_type(compiler, compstate, arg_nodes[i_arg]);
		if (arg_type == nullptr) {
			return nullptr;
		}

		uint64_t i_expr_type = uint64_t(arg_type - compstate->types);
		if (i_expr_type >= compstate->types_length) {
			compstate->result = Result::Fatal;
			compstate->error = node->span;
			return nullptr;
		}
		uint64_t i_variable = 0;
		if (!compiler_push_variable(compstate, &arg_identifiers[i_arg], i_expr_type, &i_variable)) {
			return nullptr;
		}
	}

	Type *body_type = compile_expr(compiler, compstate, body_node);

	compiler_push_opcode(compiler, compstate, OpCode::Ret);
	compiler_pop_scope(compstate);

	bool valid_return_type = type_similar(return_type, body_type);
	if (!valid_return_type) {
		compstate->result = Result::CompilerExpectedTypeGot;
		compstate->error = body_node->span;
		compstate->expected_type = return_type;
		compstate->got_type = body_type;
	}

	return nullptr;
}

Type *compile_let(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *let_token_node = node->left_child;
	const AstNode *name_node = let_token_node->right_sibling;

	if (name_node == nullptr || name_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *value_node = name_node->right_sibling;
	if (value_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		compstate->error = node->span;
		return nullptr;
	}

	const Token name_token = *name_node->atom_token;
	sv name_str = sv_substr(compstate->input.text, name_token.span);

	// -- Type checking
	fprintf(stdout, "Defining new local: %.*s\n", int(name_str.length), name_str.chars);

	// Compile the body
	Type *expr_type = compile_expr(compiler, compstate, value_node);
	if (expr_type == nullptr) {
		return nullptr;
	}

	uint64_t i_expr_type = uint64_t(expr_type - compstate->types);
	if (i_expr_type >= compstate->types_length) {
		compstate->result = Result::Fatal;
		compstate->error = node->span;
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
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *current_expr_node = begin_node->right_sibling;
	if (current_expr_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		compstate->error = node->span;
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
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *rhs_node = lhs_node->right_sibling;
	if (rhs_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		compstate->error = node->span;
		return nullptr;
	}

	if (rhs_node->right_sibling != nullptr) {
		compstate->result = Result::CompilerTooManyArgs;
		compstate->error = node->span;
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
sv compstate_builtins_str[] = {
	sv{"define", strlen("define")},
	sv{"let", strlen("let")},
	sv{"begin", strlen("begin")},
	sv{"+", strlen("+")},
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
	sv identifier_str = sv_substr(compstate->input.text, identifier.span);

	// Dispatch to the correct builtin compile function (define, iadd, etc)
	const uint64_t compstate_builtin_length = ARRAY_LENGTH(compiler_builtins);
	for (uint64_t i = 0; i < compstate_builtin_length; ++i) {
		if (sv_equals(identifier_str, compstate_builtins_str[i])) {
			return compiler_builtins[i](compiler, compstate, function_node);
		}
	}

	// If the identifier is not a builtin, generate a function call
	// -- Type checking / Compiling
	fprintf(stdout, "Calling function '%.*s'\n", int(identifier_str.length), identifier_str.chars);
	compstate->result = Result::CompilerUnknownSymbol;
	compstate->error = identifier.span;

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

Result compile_module(Compiler *compiler, sv module_name, sv input)
{
	// Build text input
	TextInput text_input = {};
	text_input.text = input;
	text_input.line_endings_length = 0;
	for (uint64_t i = 0; i < input.length; ++i) {
		if (input.chars[i] == '\n') {
			text_input.line_endings_length += 1;
		}
	}
	text_input.line_endings = (uint64_t *)calloc(text_input.line_endings_length, sizeof(uint64_t));
	text_input.line_endings_length = 0;
	for (uint64_t i = 0; i < input.length; ++i) {
		if (input.chars[i] == '\n') {
			text_input.line_endings[text_input.line_endings_length] = i;
			text_input.line_endings_length += 1;
		}
	}

	// Generate tokens
	LexerState lexer = {};
	lexer.input = text_input;

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
		fprintf(stderr, "# Lexer returned %s\n", Result_str[uint64_t(lexer.result)]);

		uint64_t error_offset = lexer.error.start;
		uint64_t error_line = 0;
		uint64_t error_col = 0;
		text_input_get_line_col(&lexer.input, error_offset, &error_line, &error_col);

		sv error_str = sv_substr(input, lexer.error);
		fprintf(stderr, "Error at (%zu:%zu): '%.*s'\n", error_line, error_col, int(error_str.length), error_str.chars);

		if (lexer.offset < lexer.input.text.length) {
			fprintf(stderr, "# Stopped at char '%c'\n", lexer.input.text.chars[lexer.offset]);
		}
		return lexer.result;
	}

	uint64_t ast_nodes_capacity = 4096;
	AstNode *ast_nodes = (AstNode *)malloc(ast_nodes_capacity * sizeof(AstNode));

	ParserState parser = {};
	parser.input = text_input;
	parser.tokens = tokens;
	parser.token_length = tokens_length;
	parser.ast_nodes = ast_nodes;
	parser.ast_nodes_capacity = ast_nodes_capacity;
	parser.ast_nodes_length = 0;
	parse_module(&parser);

	print_ast(input, parser.ast_nodes);

	if (parser.result != Result::Ok) {
		fprintf(stderr,
			"# Parser[token_length: %zu, i_current_token: %zu] returned %s\n",
			parser.token_length,
			parser.i_current_token,
			Result_str[uint64_t(parser.result)]);

		uint64_t error_offset = parser.error.start;
		uint64_t error_line = 0;
		uint64_t error_col = 0;
		text_input_get_line_col(&parser.input, error_offset, &error_line, &error_col);
		sv error_str = sv_substr(input, parser.error);
		fprintf(stderr, "Error at (%zu:%zu): '%.*s'\n", error_line, error_col, int(error_str.length), error_str.chars);

		if (parser.token_length > 0) {
			uint64_t i_last_token =
				parser.i_current_token < parser.token_length ? parser.i_current_token : parser.token_length - 1;
			Token last_token = tokens[i_last_token];
			sv last_token_str = sv_substr(parser.input.text, last_token.span);

			const char *token_kind_str = TokenKind_str[uint64_t(last_token.kind)];

			fprintf(stderr,
				"# Last seen token is %s[%.*s]\n",
				token_kind_str,
				int(last_token_str.length),
				last_token_str.chars);
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
		if (sv_equals(module->name, module_name)) {
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
	compstate.input = text_input;
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

		uint64_t error_offset = compstate.error.start;
		uint64_t error_line = 0;
		uint64_t error_col = 0;
		text_input_get_line_col(&compstate.input, error_offset, &error_line, &error_col);
		sv error_str = sv_substr(input, compstate.error);
		fprintf(stderr, "Error at (%zu:%zu): '%.*s'\n", error_line, error_col, int(error_str.length), error_str.chars);

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

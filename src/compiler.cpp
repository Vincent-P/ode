// TODO: impl field in executor
// TODO: pass local as variable ref

#include "compiler.h"
#include <corecrt_wstdio.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define SET_RESULT(x)                                                                                                  \
	x##_file = __FILE__;                                                                                               \
	x##_file_line = __LINE__;

inline constexpr uint64_t MAX_STRUCT_FIELDS = 32;

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

struct Compiler
{
	Module *modules;
	uint64_t modules_length;
	uint64_t modules_capacity;
};

struct LexicalScope
{
	sv *variables_name;
	Type **variables_type;
	uint64_t variables_length;
};

struct CompilerState
{
	Module *current_module;

	TextInput input;

	const AstNode *ast_nodes;
	uint64_t ast_nodes_length;

	// Temporary types, a new type is created for each expression
	Type *types;
	uint64_t types_capacity;
	uint64_t types_length;

	LexicalScope *scopes;
	uint64_t scopes_capacity;
	uint64_t scopes_length;

	Result result;
	const char *result_file;
	int result_file_line;
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
	bool is_operator = c == '+' || c == '-' || c == '/' || c == '*' || c == '<' || c == '>' || c == '=';
	return c == '_' || is_operator || is_lower || is_upper || is_number;
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
		fprintf(stdout, "%.*s", int(token_str.length), token_str.chars);
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
void compiler_push_opcode(Compiler *, CompilerState *compstate, OpCodeKind opcode_kind)
{
	Module *current_module = compstate->current_module;
	if (current_module->bytecode_length + 1 >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	current_module->bytecode[current_module->bytecode_length] = opcode_kind;
	current_module->bytecode_length += 1;
}

uint32_t *compiler_push_u32(Compiler *, CompilerState *compstate, uint32_t value)
{
	Module *current_module = compstate->current_module;
	uint64_t to_write = sizeof(uint32_t);
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return nullptr;
	}

	uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(current_module->bytecode + current_module->bytecode_length);
	bytecode_u32[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode_u32;
}

int32_t *compiler_push_i32(Compiler *, CompilerState *compstate, int32_t value)
{
	Module *current_module = compstate->current_module;
	uint64_t to_write = sizeof(int32_t);
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return nullptr;
	}

	int32_t *bytecode_i32 = reinterpret_cast<int32_t *>(current_module->bytecode + current_module->bytecode_length);
	bytecode_i32[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode_i32;
}

void compiler_push_sv(Compiler *, CompilerState *compstate, sv value)
{
	Module *current_module = compstate->current_module;

	uint64_t to_write = sizeof(uint32_t) + value.length * sizeof(char);
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(current_module->bytecode + current_module->bytecode_length);
	bytecode_u32[0] = uint32_t(value.length);

	uint8_t *bytecode_u8 =
		reinterpret_cast<uint8_t *>(current_module->bytecode + current_module->bytecode_length + sizeof(uint32_t));
	for (uint64_t i = 0; i < value.length; ++i)
		bytecode_u8[i] = uint8_t(value.chars[i]);
	current_module->bytecode_length += to_write;
}

Type *compiler_type_new(CompilerState *compstate)
{
	if (compstate->types_length + 1 >= compstate->types_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return nullptr;
	}
	Type *new_type = compstate->types + compstate->types_length;
	compstate->types_length += 1;
	return new_type;
}

Type *compiler_save_type_to_module(CompilerState *compstate, Type *type, uint32_t *out_type_index = nullptr)
{
	Module *current_module = compstate->current_module;
	if (current_module->types_length + 1 >= current_module->types_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return nullptr;
	}
	if (out_type_index != nullptr) {
		*out_type_index = uint32_t(current_module->types_length);
	}
	Type *module_type = current_module->types + current_module->types_length;
	*module_type = *type;
	current_module->types_length += 1;
	return module_type;
}

template <typename Lambda>
static Function *compiler_compile_function(Compiler *compiler,
	CompilerState *compstate,
	sv function_name,
	Type *return_type,
	Token *arg_identifiers,
	Type **arg_types,
	uint64_t args_length,
	Lambda compile_body_fn)
{
	Module *current_module = compstate->current_module;
	for (uint64_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, function_name)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			SET_RESULT(compstate->result);
			// Actually it should be possible to recompile a function if signature has not changed.
			return nullptr;
		}
	}

	if (current_module->functions_length + 1 >= current_module->functions_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return nullptr;
	}

	// Create a new function symbol
	Function *function = current_module->functions + current_module->functions_length;
	function->name = function_name;
	function->address = current_module->bytecode_length;
	function->is_foreign = false;
	function->return_type = return_type;

	current_module->functions_length += 1;

	// Add a debug label to identify functions easily in the bytecode
	compiler_push_opcode(compiler, compstate, OpCodeKind::DebugLabel);
	compiler_push_sv(compiler, compstate, function_name);

	// Create a variable scope
	compiler_push_scope(compiler, compstate);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint64_t i_arg = 0; i_arg < args_length; ++i_arg) {
		compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
		compiler_push_u32(compiler, compstate, uint32_t(args_length - 1 - i_arg));
	}

	for (uint64_t i_arg = 0; i_arg < args_length; ++i_arg) {
		Type *arg_type = arg_types[i_arg];
		function->arg_types[function->arg_count] = arg_types[i_arg];
		function->arg_count += 1;

		uint64_t i_variable = 0;
		if (!compiler_push_variable(compstate, &arg_identifiers[i_arg], arg_type, &i_variable)) {
			return nullptr;
		}
	}

	// <-- Compile the body
	Type *body_type = compile_body_fn();

	compiler_pop_scope(compiler, compstate);
	compiler_push_opcode(compiler, compstate, OpCodeKind::Ret);

	// Return an error if body_type compilation failed
	if (body_type == nullptr) {
		return nullptr;
	}

	bool valid_return_type = type_similar(return_type, body_type);
	if (!valid_return_type) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = return_type;
		compstate->got_type = body_type;
	}

	return function;
}

bool type_similar(Type *lhs, Type *rhs)
{
	if (lhs == nullptr || rhs == nullptr) {
		return false;
	}

	if (lhs->kind != rhs->kind) {
		return false;
	} else if (lhs->kind == TypeKind::Pointer) {
		return type_similar(lhs->data.pointee, rhs->data.pointee);
	} else if (lhs->kind == TypeKind::Struct) {
		bool same_layout = lhs->data.structure.field_count == rhs->data.structure.field_count;
		for (uint64_t i = 0; same_layout && i < lhs->data.structure.field_count; ++i) {
			same_layout = type_similar(lhs->data.structure.field_types[i], rhs->data.structure.field_types[i]);
		}
		return same_layout;
	} else {
		return true;
	}
}

static constexpr uint64_t SCOPE_MAX_VARIABLES = 16;

void compiler_push_scope(Compiler *compiler, CompilerState *compstate)
{
	if (compstate->scopes_length >= compstate->scopes_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	LexicalScope *new_scope = compstate->scopes + compstate->scopes_length;
	compstate->scopes_length += 1;

	*new_scope = {};
	new_scope->variables_name = (sv *)calloc(SCOPE_MAX_VARIABLES, sizeof(sv));
	new_scope->variables_type = (Type **)calloc(SCOPE_MAX_VARIABLES, sizeof(Type));

	compiler_push_opcode(compiler, compstate, OpCodeKind::BeginScope);
}

void compiler_pop_scope(Compiler *compiler, CompilerState *compstate)
{
	if (compstate->scopes_length == 0) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	LexicalScope *current_scope = compstate->scopes + compstate->scopes_length - 1;
	compstate->scopes_length -= 1;
	free(current_scope->variables_name);
	free(current_scope->variables_type);

	compiler_push_opcode(compiler, compstate, OpCodeKind::EndScope);
}

bool compiler_push_variable(
	CompilerState *compstate, const Token *identifier_token, Type *type, uint64_t *i_variable_out)
{
	if (type == nullptr) {
		return false;
	}

	if (compstate->scopes_length >= compstate->scopes_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return false;
	}

	LexicalScope *current_scope = compstate->scopes + compstate->scopes_length - 1;
	if (current_scope->variables_length >= SCOPE_MAX_VARIABLES) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return false;
	}

	uint64_t i_new_variable = current_scope->variables_length;
	current_scope->variables_length += 1;

	sv name_str = sv_substr(compstate->input.text, identifier_token->span);

	current_scope->variables_name[i_new_variable] = name_str;
	current_scope->variables_type[i_new_variable] = type;

	*i_variable_out = i_new_variable;
	return true;
}

bool compiler_lookup_variable(
	CompilerState *compstate, const Token *identifier_token, Type **type_out, uint64_t *i_variable_out)
{
	if (compstate->scopes_length == 0) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
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
				found_type = scope->variables_type[i_variable];
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
	const bool is_token = node != nullptr && node->atom_token != nullptr;
	const bool is_unit = node != nullptr && node->left_child == nullptr && node->atom_token == nullptr;
	const bool is_type = is_token || is_unit;
	if (node == nullptr || !is_type) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	if (is_unit) {
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Unit;
		return ty;
	}

	const Token identifier = *node->atom_token;
	sv identifier_str = sv_substr(compstate->input.text, identifier.span);

	// Search builtin types
	if (sv_equals(identifier_str, sv_from_null_terminated(TypeKind_str[static_cast<uint32_t>(TypeKind::Int)]))) {
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Int;
		ty->size = sizeof(int32_t);
		return ty;
	} else if (sv_equals(identifier_str,
				   sv_from_null_terminated(TypeKind_str[static_cast<uint32_t>(TypeKind::Bool)]))) {
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Bool;
		ty->size = sizeof(bool);
		return ty;
	} else if (sv_equals(identifier_str,
				   sv_from_null_terminated(TypeKind_str[static_cast<uint32_t>(TypeKind::Float)]))) {
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Float;
		ty->size = sizeof(float);
		return ty;
	}

	// Search named types
	for (uint32_t i_type = 0; i_type < compstate->types_length; ++i_type) {
		if (compstate->types[i_type].kind == TypeKind::Struct) {
			if (sv_equals(compstate->types[i_type].data.structure.name, identifier_str)) {
				return compstate->types + i_type;
			}
		}
	}

	compstate->result = Result::CompilerUnknownSymbol;
	SET_RESULT(compstate->result);
	compstate->error = node->span;
	return nullptr;
}

Type *compile_atom(Compiler *compiler, CompilerState *compstate, const AstNode *expr_node)
{
	const Token token = *expr_node->atom_token;
	sv token_sv = sv_substr(compstate->input.text, token.span);

	if (token.kind == TokenKind::Identifier) {
		// Refer a declared variable
		// <identifier>
		Type *ty = nullptr;
		uint64_t i_variable = 0;
		if (!compiler_lookup_variable(compstate, &token, &ty, &i_variable)) {
			compstate->result = Result::CompilerUnknownSymbol;
			SET_RESULT(compstate->result);
			compstate->error = token.span;
			return nullptr;
		}
		compiler_push_opcode(compiler, compstate, OpCodeKind::GetLocal);
		compiler_push_u32(compiler, compstate, uint32_t(i_variable));
		return ty;
	} else if (token.kind == TokenKind::Number) {
		// An integer constant
		// <number>
		int32_t token_number = sv_to_int(token_sv);
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Int;
		compiler_push_opcode(compiler, compstate, OpCodeKind::Constant);
		compiler_push_i32(compiler, compstate, token_number);
		return ty;
	} else {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
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
		// () unit value
		Type *ty = compiler_type_new(compstate);
		ty->kind = TypeKind::Unit;
		return ty;
	}
}

Type *compile_sexprs_return_last(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *current_expr_node = node;
	if (current_expr_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
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

// Defines a new function
// (define (<name> <return_type>) (<args>*) <expression>+)
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
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			return nullptr;
		}
		name_token = *identifier_node->atom_token;

		return_type_node = identifier_node->right_sibling;
		if (return_type_node == nullptr) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			return nullptr;
		}
	}

	const AstNode *arglist_node = name_node->right_sibling;
	{
		if (arglist_node->atom_token != nullptr) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return nullptr;
		}

		const AstNode *identifier_node = arglist_node->left_child;

		while (identifier_node != nullptr) {
			if (identifier_node == nullptr || identifier_node->atom_token == nullptr) {
				compstate->result = Result::CompilerExpectedIdentifier;
				SET_RESULT(compstate->result);
				compstate->error = arglist_node->span;
				return nullptr;
			}

			const AstNode *type_node = identifier_node->right_sibling;
			if (type_node == nullptr) {
				compstate->result = Result::CompilerExpectedExpr;
				SET_RESULT(compstate->result);
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
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	sv name_token_str = sv_substr(compstate->input.text, name_token.span);

	// -- Type checking
	Type *return_type = parse_type(compiler, compstate, return_type_node);
	Type *arg_types[MAX_ARGUMENTS] = {};
	for (uint64_t i_arg = 0; i_arg < args_length; ++i_arg) {
		arg_types[i_arg] = parse_type(compiler, compstate, arg_nodes[i_arg]);
	}

	Function *new_function = compiler_compile_function(compiler,
		compstate,
		name_token_str,
		return_type,
		arg_identifiers,
		arg_types,
		args_length,
		[&]() -> Type * {
			// <-- Compile the body
			Type *body_type = compile_sexprs_return_last(compiler, compstate, body_node);
			return body_type;
		});

	if (new_function == nullptr) {
		return nullptr;
	}
	return new_function->return_type;
}

// Defines a new foreign function
// (define-foreign (<name> <return_type>) (<args>))
Type *compile_define_foreign(Compiler *compiler, CompilerState *compstate, const AstNode *node)
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
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			return nullptr;
		}
		name_token = *identifier_node->atom_token;

		return_type_node = identifier_node->right_sibling;
		if (return_type_node == nullptr) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			return nullptr;
		}
	}

	const AstNode *arglist_node = name_node->right_sibling;
	{
		if (arglist_node->atom_token != nullptr) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return nullptr;
		}

		const AstNode *identifier_node = arglist_node->left_child;

		while (identifier_node != nullptr) {
			if (identifier_node == nullptr || identifier_node->atom_token == nullptr) {
				compstate->result = Result::CompilerExpectedIdentifier;
				SET_RESULT(compstate->result);
				compstate->error = arglist_node->span;
				return nullptr;
			}

			const AstNode *type_node = identifier_node->right_sibling;
			if (type_node == nullptr) {
				compstate->result = Result::CompilerExpectedExpr;
				SET_RESULT(compstate->result);
				compstate->error = arglist_node->span;
				return nullptr;
			}

			arg_identifiers[args_length] = *identifier_node->atom_token;
			arg_nodes[args_length] = type_node;
			args_length += 1;

			identifier_node = type_node->left_child;
		}
	}

	if (arglist_node->right_sibling != nullptr) {
		compstate->result = Result::CompilerUnexpectedExpression;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	sv name_token_str = sv_substr(compstate->input.text, name_token.span);

	// -- Type checking
	Module *current_module = compstate->current_module;
	for (uint64_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, name_token_str)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			// Actually it should be possible to recompile a function if signature has not changed.
			return nullptr;
		}
	}

	if (current_module->functions_length + 1 >= current_module->functions_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	// Create a new function symbol
	Function *function = current_module->functions + current_module->functions_length;
	function->name = name_token_str;
	function->address = current_module->bytecode_length;
	function->is_foreign = true;

	current_module->functions_length += 1;

	// -- Compiling
	Type *return_type = parse_type(compiler, compstate, return_type_node);
	if (return_type == nullptr) {
		return nullptr;
	}
	function->return_type = return_type;

	// Add a debug label to identify functions easily in the bytecode
	compiler_push_opcode(compiler, compstate, OpCodeKind::DebugLabel);
	compiler_push_sv(compiler, compstate, name_token_str);

	// Create a variable scope
	compiler_push_scope(compiler, compstate);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint64_t i_arg = 0; i_arg < args_length; ++i_arg) {
		compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
		compiler_push_u32(compiler, compstate, uint32_t(args_length - 1 - i_arg));
	}

	for (uint64_t i_arg = 0; i_arg < args_length; ++i_arg) {
		Type *arg_type = parse_type(compiler, compstate, arg_nodes[i_arg]);
		function->arg_types[function->arg_count] = arg_type;
		function->arg_count += 1;

		uint64_t i_variable = 0;
		if (!compiler_push_variable(compstate, &arg_identifiers[i_arg], arg_type, &i_variable)) {
			return nullptr;
		}
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::CallForeign);

	// <-- Compile the body
	compiler_pop_scope(compiler, compstate);
	compiler_push_opcode(compiler, compstate, OpCodeKind::Ret);

	return return_type;
}

// Defines a new struct
// (struct <name> (<field name> <field type>)+)
Type *compile_struct(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *struct_token_node = node->left_child;

	const AstNode *identifier_node = struct_token_node->right_sibling;
	if (identifier_node == nullptr || identifier_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}
	Token name_token = *identifier_node->atom_token;

	Token field_identifiers[MAX_STRUCT_FIELDS] = {};
	const AstNode *field_type_nodes[MAX_STRUCT_FIELDS] = {};
	uint64_t fields_length = 0;

	const AstNode *field_node = identifier_node->right_sibling;
	if (field_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	while (field_node != nullptr) {
		const AstNode *field_identifier_node = field_node->left_child;

		if (field_identifier_node == nullptr || field_identifier_node->atom_token == nullptr) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = field_identifier_node->span;
			return nullptr;
		}

		const AstNode *type_node = field_identifier_node->right_sibling;
		if (type_node == nullptr) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = field_node->span;
			return nullptr;
		}

		if (fields_length > MAX_STRUCT_FIELDS) {
			compstate->result = Result::Fatal;
			SET_RESULT(compstate->result);
			compstate->error = field_node->span;
			return nullptr;
		}

		field_identifiers[fields_length] = *field_identifier_node->atom_token;
		field_type_nodes[fields_length] = type_node;
		fields_length += 1;

		field_node = field_node->right_sibling;
	}

	sv name_token_str = sv_substr(compstate->input.text, name_token.span);

	// -- Type checking
	for (uint64_t i_type = 0; i_type < compstate->types_length; ++i_type) {
		Type *type = compstate->types + i_type;
		if (type->kind == TypeKind::Struct && sv_equals(type->data.structure.name, name_token_str)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			// Actually it should be possible to recompile a function if signature has not changed.
			return nullptr;
		}
	}

	if (compstate->types_length + 1 >= compstate->types_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	// -- Create a new structure type
	Type *fields_type[MAX_STRUCT_FIELD] = {};

	Type *struct_type = compiler_type_new(compstate);
	struct_type->kind = TypeKind::Struct;
	struct_type->size = 0;
	struct_type->data.structure = {};
	struct_type->data.structure.name = name_token_str;

	uint64_t struct_size = 0;
	for (uint64_t i_field = 0; i_field < fields_length; ++i_field) {
		Type *field_type = parse_type(compiler, compstate, field_type_nodes[i_field]);
		if (field_type == nullptr) {
			return nullptr;
		}

		struct_type->data.structure.field_types[i_field] = field_type;
		struct_type->data.structure.field_names[i_field] =
			sv_substr(compstate->input.text, field_identifiers[i_field].span);
		struct_type->data.structure.field_offsets[i_field] = struct_size;

		struct_size += field_type->size;
		fields_type[i_field] = field_type;
	}

	struct_type->data.structure.field_count = fields_length;
	struct_type->size = struct_size;

	uint32_t struct_type_index = 0;
	struct_type = compiler_save_type_to_module(compstate, struct_type, &struct_type_index);

	// -- Compile builtin-functions for the struct
	sv ctor_name = name_token_str;
	/*Function *ctor =*/compiler_compile_function(compiler,
		compstate,
		ctor_name,
		struct_type,
		field_identifiers,
		fields_type,
		fields_length,
		[&]() -> Type * {
			/*
		            result = struct_type{}
		            result.field0 = arg0
		            result.field1 = arg1
		            result.field2 = arg2
		            return local0
		    */

			uint32_t struct_local_index = uint32_t(fields_length);

			// result = MyStruct{}
			compiler_push_opcode(compiler, compstate, OpCodeKind::MakeStruct);
			compiler_push_u32(compiler, compstate, struct_type_index);
			compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
			compiler_push_u32(compiler, compstate, struct_local_index);

			for (uint64_t i_field = 0; i_field < fields_length; ++i_field) {
				// result.field = arg_i

				// result
				compiler_push_opcode(compiler, compstate, OpCodeKind::GetLocal);
				compiler_push_u32(compiler, compstate, struct_local_index);

				// arg_i
				compiler_push_opcode(compiler, compstate, OpCodeKind::GetLocal);
				compiler_push_u32(compiler, compstate, uint32_t(i_field));

				// set field
				compiler_push_opcode(compiler, compstate, OpCodeKind::SetField);
				compiler_push_u32(compiler, compstate, uint32_t(i_field));
			}

			// return result
			compiler_push_opcode(compiler, compstate, OpCodeKind::GetLocal);
			compiler_push_u32(compiler, compstate, struct_local_index);

			return struct_type;
		});

	return struct_type;
}

// Conditional branch
// (if <cond_expression> <then_expression> <else_expression>)
Type *compile_if(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *if_token_node = node->left_child;
	// TODO: How to write this better?
	if (if_token_node == nullptr || if_token_node->right_sibling == nullptr ||
		if_token_node->right_sibling->right_sibling == nullptr ||
		if_token_node->right_sibling->right_sibling->right_sibling == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *cond_expr_node = if_token_node->right_sibling;
	const AstNode *then_expr_node = cond_expr_node->right_sibling;
	const AstNode *else_expr_node = then_expr_node->right_sibling;

	// Compile the condition first,
	Type *cond_expr = compile_expr(compiler, compstate, cond_expr_node);
	if (cond_expr == nullptr) {
		return nullptr;
	}

	// If true, jump to the true branch (patch the jump adress later)
	compiler_push_opcode(compiler, compstate, OpCodeKind::ConditionalJump);
	uint32_t *jump_to_true_branch = compiler_push_u32(compiler, compstate, 0);

	// Then compile the else branch, because the condition was false
	Type *else_expr = compile_expr(compiler, compstate, else_expr_node);
	if (else_expr == nullptr) {
		return nullptr;
	}

	// Jump over the true branch (patch the jump adress later)
	compiler_push_opcode(compiler, compstate, OpCodeKind::Jump);
	uint32_t *jump_to_end = compiler_push_u32(compiler, compstate, 0);

	// Compile the true branch
	const uint64_t then_branch_address = compstate->current_module->bytecode_length;
	Type *then_expr = compile_expr(compiler, compstate, then_expr_node);
	if (then_expr == nullptr) {
		return nullptr;
	}

	const uint64_t end_address = compstate->current_module->bytecode_length;
	*jump_to_true_branch = uint32_t(then_branch_address);
	*jump_to_end = uint32_t(end_address);

	bool valid_return_type = type_similar(then_expr, else_expr);
	if (!valid_return_type) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		compstate->expected_type = then_expr;
		compstate->got_type = else_expr;
	}

	return then_expr;
}

Type *compile_let(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *let_token_node = node->left_child;
	const AstNode *name_node = let_token_node->right_sibling;

	if (name_node == nullptr || name_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *value_node = name_node->right_sibling;
	if (value_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	const Token name_token = *name_node->atom_token;

	// -- Type checking
	// Compile the body
	Type *expr_type = compile_expr(compiler, compstate, value_node);
	uint64_t i_variable = 0;
	if (!compiler_push_variable(compstate, &name_token, expr_type, &i_variable)) {
		return nullptr;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
	compiler_push_u32(compiler, compstate, uint32_t(i_variable));

	return expr_type;
}

Type *compile_begin(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *begin_node = node->left_child;
	if (begin_node == nullptr || begin_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	return compile_sexprs_return_last(compiler, compstate, begin_node->right_sibling);
}

// (<op> <lhs> <rhs>)
Type *compile_binary_opcode(
	Compiler *compiler, CompilerState *compstate, const AstNode *node, Type *type, OpCodeKind opcode)
{
	// -- Parsing
	const AstNode *plus_token_node = node->left_child;

	const AstNode *lhs_node = plus_token_node->right_sibling;
	if (lhs_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *rhs_node = lhs_node->right_sibling;
	if (rhs_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	if (rhs_node->right_sibling != nullptr) {
		compstate->result = Result::CompilerTooManyArgs;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	// -- Type checking
	Type *lhs = compile_expr(compiler, compstate, lhs_node);
	Type *rhs = compile_expr(compiler, compstate, rhs_node);
	if (compstate->result != Result::Ok) {
		return nullptr;
	}

	bool lhs_valid = type_similar(lhs, type);
	bool rhs_valid = type_similar(rhs, type);

	if (!lhs_valid) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = type;
		compstate->got_type = lhs;
		compstate->error = lhs_node->span;
		return nullptr;
	}

	if (!rhs_valid) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = type;
		compstate->got_type = rhs;
		compstate->error = rhs_node->span;
		return nullptr;
	}

	compiler_push_opcode(compiler, compstate, opcode);

	return lhs;
}

// Add two integers
Type *compile_iadd(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	Type *int_type = compiler_type_new(compstate);
	int_type->kind = TypeKind::Int;
	return compile_binary_opcode(compiler, compstate, node, int_type, OpCodeKind::IAdd);
}

// Substract two integers
Type *compile_isub(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	Type *int_type = compiler_type_new(compstate);
	int_type->kind = TypeKind::Int;
	return compile_binary_opcode(compiler, compstate, node, int_type, OpCodeKind::ISub);
}

// Compare two integers
// (<= <lhs> <rhs>)
Type *compile_ltethan(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	Type *int_type = compiler_type_new(compstate);
	int_type->kind = TypeKind::Int;
	return compile_binary_opcode(compiler, compstate, node, int_type, OpCodeKind::ILessThanEq);
}

// Returns a field of a struct
// (field <expr> <member identifier>)
Type *compile_field(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *field_token_node = node->left_child;
	if (field_token_node == nullptr || field_token_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *expr_node = field_token_node->right_sibling;
	if (expr_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	const AstNode *field_identifier_node = expr_node->right_sibling;
	if (field_identifier_node == nullptr || field_identifier_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	// Typecheck
	Type *expr_type = compile_expr(compiler, compstate, expr_node);
	if (expr_type == nullptr) {
		return nullptr;
	}

	if (expr_type->kind != TypeKind::Struct) {
		compstate->result = Result::CompilerExpectedStruct;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	const Token field_id = *field_identifier_node->atom_token;
	const sv field_id_str = sv_substr(compstate->input.text, field_id.span);

	uint64_t field_count = expr_type->data.structure.field_count;
	uint64_t i_found_field = ~0llu;

	for (uint64_t i_field = 0; i_field < field_count; ++i_field) {
		if (sv_equals(expr_type->data.structure.field_names[i_field], field_id_str)) {
			i_found_field = i_field;
			break;
		}
	}

	if (i_found_field == ~0llu) {
		compstate->result = Result::CompilerUnknownField;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return nullptr;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::GetField);
	compiler_push_u32(compiler, compstate, uint32_t(i_found_field));

	Type *field_type = expr_type->data.structure.field_types[i_found_field];
	return field_type;
}

using CompstateBuiltin = Type *(*)(Compiler *, CompilerState *, const AstNode *);

// There are two kinds of "builtins", the ones allowed at top-level and the other ones
const CompstateBuiltin compiler_top_builtins[] = {
	compile_define,
	compile_define_foreign,
	compile_struct,
};
const sv compiler_top_builtins_str[] = {
	sv_from_null_terminated("define"),
	sv_from_null_terminated("define-foreign"),
	sv_from_null_terminated("struct"),
};
static_assert(ARRAY_LENGTH(compiler_top_builtins_str) == ARRAY_LENGTH(compiler_top_builtins));

const CompstateBuiltin compiler_expr_builtins[] = {
	compile_if,
	compile_let,
	compile_begin,
	compile_iadd,
	compile_isub,
	compile_ltethan,
	compile_field,
};
const sv compiler_expr_builtins_str[] = {
	sv_from_null_terminated("if"),
	sv_from_null_terminated("let"),
	sv_from_null_terminated("begin"),
	sv_from_null_terminated("+"),
	sv_from_null_terminated("-"),
	sv_from_null_terminated("<="),
	sv_from_null_terminated("field"),
};
static_assert(ARRAY_LENGTH(compiler_expr_builtins_str) == ARRAY_LENGTH(compiler_expr_builtins));

// (<identifier> <expr>*)
Type *compile_sexpr(Compiler *compiler, CompilerState *compstate, const AstNode *function_node)
{
	const AstNode *identifier_node = function_node->left_child;
	if (identifier_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		return nullptr;
	}

	const Token identifier = *identifier_node->atom_token;
	sv identifier_str = sv_substr(compstate->input.text, identifier.span);

	// Dispatch to the correct builtin compile function (define, iadd, etc)
	const uint64_t compstate_builtin_length = ARRAY_LENGTH(compiler_expr_builtins);
	for (uint64_t i = 0; i < compstate_builtin_length; ++i) {
		if (sv_equals(identifier_str, compiler_expr_builtins_str[i])) {
			return compiler_expr_builtins[i](compiler, compstate, function_node);
		}
	}

	// If the identifier is not a builtin, generate a function call
	Module *current_module = compstate->current_module;
	Function *found_function = nullptr;
	uint64_t i_function = 0;
	for (; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, identifier_str)) {
			found_function = function;
			break;
		}
	}

	// Function not found
	if (found_function == nullptr) {
		compstate->result = Result::CompilerUnknownSymbol;
		SET_RESULT(compstate->result);
		compstate->error = identifier.span;
		return nullptr;
	}

	// Typecheck arguments
	uint32_t i_sig_arg_type = 0;
	const AstNode *arg_node = identifier_node->right_sibling;
	while (arg_node != nullptr) {
		// There is an expr but the signature has ended
		if (i_sig_arg_type >= found_function->arg_count) {
			compstate->result = Result::CompilerUnexpectedExpression;
			SET_RESULT(compstate->result);
			compstate->error = arg_node->span;
			return nullptr;
		}

		Type *arg_type = compile_expr(compiler, compstate, arg_node);
		if (arg_type == nullptr) {
			return nullptr;
		}

		if (!type_similar(arg_type, found_function->arg_types[i_sig_arg_type])) {
			compstate->result = Result::CompilerExpectedTypeGot;
			SET_RESULT(compstate->result);
			compstate->error = arg_node->span;
			compstate->expected_type = found_function->arg_types[i_sig_arg_type];
			compstate->got_type = arg_type;
		}

		arg_node = arg_node->right_sibling;
		i_sig_arg_type += 1;
	}

	// There is an argument left in the signature
	if (i_sig_arg_type < found_function->arg_count) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = function_node->span;
		compstate->expected_type = found_function->arg_types[i_sig_arg_type];
		compstate->got_type = nullptr;
		return nullptr;
	}

	// The found function signature matched
	compiler_push_opcode(compiler, compstate, OpCodeKind::Call);
	compiler_push_u32(compiler, compstate, uint32_t(i_function));

	return found_function->return_type;
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
			SET_RESULT(compstate->result);
			return;
		}

		sv identifier_str = sv_substr(compstate->input.text, first_sexpr_member->atom_token->span);

		uint64_t i_builtin = 0;
		for (; i_builtin < ARRAY_LENGTH(compiler_top_builtins); ++i_builtin) {
			if (sv_equals(identifier_str, compiler_top_builtins_str[i_builtin])) {
				compiler_top_builtins[i_builtin](compiler, compstate, root_expr);
				break;
			}
		}

		if (i_builtin >= ARRAY_LENGTH(compiler_top_builtins)) {
			compstate->result = Result::CompilerUnknownSymbol;
			SET_RESULT(compstate->result);
			compstate->error = first_sexpr_member->span;
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
}

void module_init(Module *new_module, sv module_name)
{
	const uint64_t functions_capacity = 8;
	new_module->functions_capacity = functions_capacity;
	new_module->functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));

	const uint64_t bytecode_capacity = 1024;
	new_module->bytecode_capacity = bytecode_capacity;
	new_module->bytecode = static_cast<uint8_t *>(calloc(bytecode_capacity, sizeof(OpCodeKind)));

	const uint64_t types_capacity = 8;
	new_module->types_capacity = types_capacity;
	new_module->types = static_cast<Type *>(calloc(types_capacity, sizeof(Type)));

	new_module->name = module_name;
}

Result compile_module(Compiler *compiler, sv module_name, sv input, Module **out_module)
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

#if 0
	fprintf(stdout, "\nParsing success:\n");
	print_ast(input, parser.ast_nodes);
#endif

	LexicalScope scopes[16] = {};

	Module new_module = {};
	module_init(&new_module, module_name);

	CompilerState compstate = {};
	compstate.current_module = &new_module;
	compstate.input = text_input;
	compstate.ast_nodes = parser.ast_nodes;
	compstate.ast_nodes_length = parser.ast_nodes_length;
	compstate.types_capacity = 64;
	compstate.types = (Type *)calloc(compstate.types_capacity, sizeof(Type));
	compstate.scopes = scopes;
	compstate.scopes_capacity = 16;
	compile_module(compiler, &compstate);

	if (compstate.result != Result::Ok) {
		fprintf(stderr,
			"%s(%d) Compstate[] returned %s\n",
			compstate.result_file,
			compstate.result_file_line,
			Result_str[uint64_t(compstate.result)]);

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

	fprintf(stdout, "\nCompilation success:\n");
	fprintf(stdout, "Exported types: %zu\n", new_module.types_length);
	for (uint64_t offset = 0; offset < new_module.bytecode_length; ++offset) {
		uint8_t opcode = new_module.bytecode[offset];
		if (opcode >= OpCodeKind::Count) {
			break;
		}

		OpCodeKind opcode_kind = OpCodeKind(opcode);
		fprintf(stdout, "%zu\t%s", offset, OpCode_str[uint8_t(opcode_kind)]);

		bool is_unary_u32 = opcode_kind == OpCodeKind::SetLocal || opcode_kind == OpCodeKind::GetLocal ||
		                    opcode_kind == OpCodeKind::GetField || opcode_kind == OpCodeKind::SetField ||
		                    opcode_kind == OpCodeKind::Call || opcode_kind == OpCodeKind::MakeStruct;

		bool is_unary_i32 = opcode_kind == OpCodeKind::Constant || opcode_kind == OpCodeKind::Jump ||
		                    opcode_kind == OpCodeKind::ConditionalJump;

		bool is_unary_sv = opcode_kind == OpCodeKind::DebugLabel;

		if (is_unary_u32) {
			uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(new_module.bytecode + offset + 1);
			fprintf(stdout, " %u", bytecode_u32[0]);
			offset += sizeof(uint32_t);
		} else if (is_unary_i32) {
			int32_t *bytecode_i32 = reinterpret_cast<int32_t *>(new_module.bytecode + offset + 1);
			fprintf(stdout, " %d", bytecode_i32[0]);
			offset += sizeof(int32_t);
		} else if (is_unary_sv) {
			uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(new_module.bytecode + offset + 1);
			uint32_t sv_length = bytecode_u32[0];
			offset += sizeof(uint32_t);

			char *bytecode_chars = reinterpret_cast<char *>(bytecode_u32 + 1);
			char buffer[128] = {};
			for (uint32_t i = 0; i < sv_length && i < 127; ++i) {
				buffer[i] = bytecode_chars[i];
			}
			offset += sv_length;

			fprintf(stdout, " %s", buffer);
		}

		fprintf(stdout, "\n");
	}

	// Everything went well, copy the new compiled module to the persistant compiler state
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
		compiler->modules_length += 1;
	}

	compiler->modules[i_module] = new_module;
	*out_module = compiler->modules + i_module;

	return Result::Ok;
}

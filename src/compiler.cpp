#include "compiler.h"
#include <corecrt_wstdio.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

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
	uint64_t i_current_token;
	TokenKind expected_token_kind;
};

struct Function
{
	const char *name;
	uint64_t name_length;
	uint64_t address; // offset into the compiler bytecode
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

struct CompilerState
{
	uint64_t i_current_module;
	const char *input;
	uint64_t input_length;
	const Token *tokens;
	uint64_t token_length;
	const AstNode *ast_nodes;
	uint64_t ast_nodes_length;
	Result result;
	Token got_token;
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
	}

	lexer->offset = uint64_t(input_cursor - lexer->input);
}

Token parser_current_token(ParserState *parser)
{
	if (parser->i_current_token < parser->token_length) {
		return parser->tokens[parser->i_current_token];
	} else {
		parser->result = Result::ExpectedTokenGotEof;
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
		return {};
	}

	Token token = parser->tokens[parser->i_current_token];

	if (token.kind == expect_kind) {
		parser->i_current_token += 1;
		parser->result = Result::Ok;
	} else {
		parser->result = Result::UnexpectedToken;
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

void compile_sexpr(Compiler *compiler, CompilerState *compstate, const AstNode *function_node);
void compile_expr(Compiler *compiler, CompilerState *compstate, const AstNode *expr_node)
{
	if (expr_node->atom_token != nullptr) {
		const Token token = *expr_node->atom_token;
		if (token.kind == TokenKind::Identifier) {
			fprintf(stdout, "Expr evaluated to %.*s\n", int(token.length), compstate->input + token.offset);
			compiler_push_opcode(compiler, compstate, OpCode::GetLocal);
		} else if (token.kind == TokenKind::Number) {
			int token_number = int_from_string(compstate->input + token.offset, token.length);
			fprintf(stdout, "Expr evaluated to %d\n", token_number);
			compiler_push_opcode(compiler, compstate, OpCode::Constant);
		} else {
			compstate->result = Result::Fatal;
		}
	} else if (expr_node->left_child != nullptr) {
		compile_sexpr(compiler, compstate, expr_node);
	} else {
		compstate->result = Result::Fatal;
	}
}

void compile_define(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *define_token_node = node->left_child;
	const AstNode *name_node = define_token_node->right_sibling;

	if (name_node == nullptr || name_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return;
	}

	const AstNode *return_type_node = name_node->right_sibling;
	if (return_type_node == nullptr || return_type_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return;
	}

	const AstNode *body_node = return_type_node->right_sibling;
	if (body_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return;
	}

	const Token name_token = *name_node->atom_token;
	const char *name_token_str = compstate->input + name_token.offset;
	const Token return_token = *return_type_node->atom_token;

	// Define new function
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
			return;
		}
	}

	if (current_module->functions_length + 1 >= current_module->functions_capacity) {
		compstate->result = Result::Fatal;
		return;
	}

	Function *function = current_module->functions + current_module->functions_length;
	current_module->functions_length += 1;

	function->name = name_token_str;
	function->name_length = name_token.length;
	function->address = current_module->bytecode_length;

	// TODO arguments

	// Compile the body
	compile_expr(compiler, compstate, body_node);

	compiler_push_opcode(compiler, compstate, OpCode::Ret);
}

void compile_iadd(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *plus_token_node = node->left_child;

	const AstNode *lhs_node = plus_token_node->right_sibling;
	if (lhs_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return;
	}

	const AstNode *rhs_node = lhs_node->right_sibling;
	if (rhs_node == nullptr) {
		compstate->result = Result::CompilerExpectedExpr;
		return;
	}

	if (rhs_node->right_sibling != nullptr) {
		compstate->result = Result::CompilerTooManyArgs;
		return;
	}

	compile_expr(compiler, compstate, lhs_node);
	compile_expr(compiler, compstate, rhs_node);
	compiler_push_opcode(compiler, compstate, OpCode::IAdd);
}

using CompstateBuiltin = void (*)(Compiler *, CompilerState *, const AstNode *);
CompstateBuiltin compiler_builtins[] = {
	compile_define,
	compile_iadd,
};
const char *compstate_builtins_str[] = {
	"define",
	"+",
};
static_assert(ARRAY_LENGTH(Result_str) == uint64_t(Result::Count));

void compile_sexpr(Compiler *compiler, CompilerState *compstate, const AstNode *function_node)
{
	const AstNode *identifier_node = function_node->left_child;
	if (identifier_node->atom_token == nullptr) {
		compstate->result = Result::CompilerExpectedIdentifier;
		return;
	}

	const Token identifier = *identifier_node->atom_token;
	const char *identifier_string = compstate->input + identifier.offset;

	// Lookup for code generation "intrinsics"
	const uint64_t compstate_builtin_length = ARRAY_LENGTH(compiler_builtins);
	for (uint64_t i = 0; i < compstate_builtin_length; ++i) {
		if (string_equals(identifier_string,
				identifier.length,
				compstate_builtins_str[i],
				strlen(compstate_builtins_str[i]))) {
			compiler_builtins[i](compiler, compstate, function_node);
			return;
		}
	}

	// Generate function call
	fprintf(stdout, "Calling function '%.*s'\n", int(identifier.length), identifier_string);
	// TODO lookup symbol
	// compile error
	compstate->result = Result::CompilerUnknownSymbol;

	// evalutate args
	const AstNode *arg_node = identifier_node->right_sibling;
	while (arg_node != nullptr) {
		compile_expr(compiler, compstate, arg_node);
		arg_node = arg_node->right_sibling;
	}
}

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
		compiler->modules_length += 1;
		const uint64_t functions_capacity = 8;
		const uint64_t bytecode_capacity = 1024;
		new_module->functions_capacity = functions_capacity;
		new_module->functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));
		new_module->bytecode_capacity = functions_capacity;
		new_module->bytecode = static_cast<OpCode *>(calloc(bytecode_capacity, sizeof(OpCode)));
	}

	CompilerState compstate = {};
	compstate.i_current_module = i_module;
	compstate.input = parser.input;
	compstate.input_length = parser.input_length;
	compstate.tokens = parser.tokens;
	compstate.token_length = parser.token_length;
	compstate.ast_nodes = parser.ast_nodes;
	compstate.ast_nodes_length = parser.ast_nodes_length;
	compile_module(compiler, &compstate);

	if (compstate.result != Result::Ok) {
		fprintf(stderr, "# Compstate[] returned %s\n", Result_str[uint64_t(compstate.result)]);
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

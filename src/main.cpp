#define _CRT_SECURE_NO_WARNINGS
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_LENGTH(x) sizeof(x) / sizeof(x[0])

char *read_entire_file(FILE *file, long *out_size)
{
	fseek(file, 0, SEEK_END);
	long fsize = ftell(file);
	fseek(file, 0, SEEK_SET);

	char *file_content = (char *)malloc(uint64_t(fsize) + 1);
	fread(file_content, uint64_t(fsize), 1, file);
	file_content[fsize] = 0;

	if (out_size)
		*out_size = fsize;

	return file_content;
}

bool string_equals(const char *string1, uint64_t length1, const char *string2, uint64_t length2)
{
	if (length1 != length2) {
		return false;
	}

	const char *end1 = string1 + length1;
	for (; string1 < end1;) {
		if (*string1 != *string2) {
			return false;
		}
		string1 += 1;
		string2 += 1;
	}

	return true;
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

enum struct Result : uint64_t
{
	Ok,
	LexerError,
	LexerDone,
	UnexpectedToken,
	ExpectedTokenGotEof,
	Count,
};
const char *Result_str[] = {
	"Ok",
	"LexerError",
	"LexerDone",
	"UnexpectedToken",
	"ExpectedTokenGotEof",
};
static_assert(ARRAY_LENGTH(Result_str) == uint64_t(Result::Count));

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
	return c == ' ' || c == '\n' || c == '\r';
}

Result lexer_next_token(LexerState *lexer, Token *result)
{
	const char *input_cursor = lexer->input + lexer->offset;
	const char *input_end = lexer->input + lexer->input_length;

	while (input_cursor < input_end && is_whitespace(*input_cursor)) {
		input_cursor += 1;
	}

	if (input_cursor >= input_end) {
		*result = {};
		return Result::LexerDone;
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
		return Result::LexerError;
	}

	lexer->offset = uint64_t(input_cursor - lexer->input);
	return Result::Ok;
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

Token parser_expect_token(ParserState *parser, TokenKind token_kind)
{
	if (parser->result != Result::Ok) {
		return {};
	}

	if (parser->i_current_token >= parser->token_length) {
		parser->result = Result::ExpectedTokenGotEof;
		return {};
	}

	Token token = parser->tokens[parser->i_current_token];

	if (token.kind == token_kind) {
		parser->i_current_token += 1;
		parser->result = Result::Ok;
	} else {
		parser->result = Result::UnexpectedToken;
		parser->expected_token_kind = token_kind;
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
		printf("%s[%.*s]", TokenKind_str[uint64_t(token.kind)], int(token.length), input + token.offset);
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

int main(int, const char *argv[])
{
	const char *input_file_path = argv[1];
	FILE *input_file = fopen(input_file_path, "r");
	long file_size = 0;
	char *file_content = read_entire_file(input_file, &file_size);
	fclose(input_file);

	printf("Running: %s\n", input_file_path);
	printf("%s\n", file_content);

	LexerState lexer = {};
	lexer.input = file_content;
	lexer.input_length = uint64_t(file_size);

	uint64_t tokens_capacity = 4096;
	Token *tokens = (Token *)malloc(tokens_capacity * sizeof(Token));
	uint64_t tokens_length = 0;

	while (tokens_length < tokens_capacity && lexer_next_token(&lexer, &tokens[tokens_length]) == Result::Ok) {
		tokens_length += 1;
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

	printf("Parsing returned %s\n", Result_str[uint64_t(parser.result)]);

	print_ast(lexer.input, parser.ast_nodes);

	if (parser.result != Result::Ok) {
		printf("Parser[token_length: %zu, i_current_token: %zu]\n", parser.token_length, parser.i_current_token);

		if (parser.token_length > 0) {
			uint64_t i_last_token =
				parser.i_current_token < parser.token_length ? parser.i_current_token : parser.token_length - 1;
			Token last_token = tokens[i_last_token];
			const char *token_kind_str = TokenKind_str[uint64_t(last_token.kind)];
			printf("Last seen token is %s '%.*s'\n",
				token_kind_str,
				int(last_token.length),
				lexer.input + last_token.offset);
		}

		if (parser.expected_token_kind != TokenKind::Invalid) {
			printf("Expected token of kind %s\n", TokenKind_str[uint64_t(parser.expected_token_kind)]);
		}
	}

	return 0;
}

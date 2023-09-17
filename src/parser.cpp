#include "parser.h"
#include "lexer.h"
#include <stdio.h>


static Token parser_current_token(Parser *parser)
{
	if (parser->i_current_token < parser->tokens.length) {
		return *vec_at(&parser->tokens, parser->i_current_token);
	} else {
		parser->result = ParserResult::ExpectedTokenGotEof;
		parser->error = {};
		if (parser->tokens.length > 0) {
			const Token *last_token = vec_at(&parser->tokens, parser->tokens.length - 1);
			parser->error = last_token->span;
		}
		return {};
	}
}

static Token parser_expect_token(Parser *parser, TokenKind expect_kind)
{
	if (parser->result != ParserResult::Ok) {
		return {};
	}

	if (parser->i_current_token >= parser->tokens.length) {
		parser->result = ParserResult::ExpectedTokenGotEof;
		parser->error = {};
		if (parser->tokens.length > 0) {
			const Token *last_token = vec_at(&parser->tokens, parser->tokens.length - 1);
			parser->error = last_token->span;
		}
		return {};
	}

	Token token = *vec_at(&parser->tokens, parser->i_current_token);

	if (token.kind == expect_kind) {
		parser->i_current_token += 1;
		parser->result = ParserResult::Ok;
	} else {
		parser->result = ParserResult::UnexpectedToken;
		parser->error = token.span;
		parser->expected_token_kind = expect_kind;
	}
	return token;
}

static uint32_t parser_push_ast_node_atom(Parser *parser, uint32_t token_index)
{
	AstNode new_node = {};
	new_node.atom_token_index = token_index;
	new_node.left_child_index = INVALID_NODE_INDEX;
	new_node.right_sibling_index = INVALID_NODE_INDEX;
	return vec_append(&parser->nodes, new_node);
}

static uint32_t parser_push_ast_node_sexpr(Parser *parser, uint32_t first_child_index)
{
	AstNode new_node = {};
	new_node.atom_token_index = INVALID_NODE_INDEX;
	new_node.right_sibling_index = INVALID_NODE_INDEX;
	new_node.left_child_index = first_child_index;
	return vec_append(&parser->nodes, new_node);
}

#if 0
static void ast_node_add_child(vec<AstNode> *nodes, uint32_t node_index, uint32_t new_child_index)
{
	AstNode *node = vec_at(nodes, node_index);
	if (node->left_child_index == INVALID_NODE_INDEX) {
		node->left_child_index = new_child_index;
	} else {
		AstNode *child_cursor = vec_at(nodes, node->left_child_index);
		while (child_cursor->right_sibling_index != INVALID_NODE_INDEX) {
			child_cursor = vec_at(nodes, child_cursor->right_sibling_index);
		}
		child_cursor->right_sibling_index = new_child_index;
	}
}
#endif

static void ast_node_add_sibling(AstNode *child, uint32_t new_sibling_index)
{
	child->right_sibling_index = new_sibling_index;
}

static void print_indent(int indent)
{
	for (int i = 0; i < indent; ++i) {
		putchar(' ');
		putchar(' ');
	}
}

static void print_ast_rec(
	sv input, const vec<Token> *tokens, const vec<AstNode> *nodes, uint32_t node_index, int indent)
{
	const AstNode *node = vec_at(nodes, node_index);
	if (node->left_child_index != INVALID_NODE_INDEX) {
		putchar('(');
	}

	if (node->atom_token_index != INVALID_NODE_INDEX) {
		const Token *token = vec_at(tokens, node->atom_token_index);
		sv token_str = sv_substr(input, token->span);
		fprintf(stdout, "%.*s", int(token_str.length), token_str.chars);
	}

	if (node->left_child_index != INVALID_NODE_INDEX) {
		const AstNode *child = vec_at(nodes, node->left_child_index);
		print_ast_rec(input, tokens, nodes, node->left_child_index, indent);

		while (child->right_sibling_index != INVALID_NODE_INDEX) {
			uint32_t next_child_index = child->right_sibling_index;
			putchar('\n');
			print_indent(indent + 1);
			print_ast_rec(input, tokens, nodes, next_child_index, indent + 1);
			child = vec_at(nodes, next_child_index);
		}

		putchar(')');
	}
}

void print_ast(sv input, const vec<Token> *tokens, const vec<AstNode> *nodes, uint32_t root_index)
{
	const AstNode *root = vec_at(nodes, root_index);
	if (root_index >= nodes->length || root->left_child_index == INVALID_NODE_INDEX) {
		return;
	}

	const AstNode *child = vec_at(nodes, root->left_child_index);
	print_ast_rec(input, tokens, nodes, root->left_child_index, 0);
	while (child->right_sibling_index != INVALID_NODE_INDEX) {
		putchar('\n');
		print_ast_rec(input, tokens, nodes, child->right_sibling_index, 0);
		child = vec_at(nodes, child->right_sibling_index);
	}
	putchar('\n');
}

// number | identifier
static uint32_t parse_atom(Parser *parser)
{
	Token current_token = parser_current_token(parser);

	bool is_a_valid_atom = current_token.kind == TokenKind::Number || current_token.kind == TokenKind::Identifier ||
	                       current_token.kind == TokenKind::StringLiteral;
	if (is_a_valid_atom) {
		uint32_t new_node_index = parser_push_ast_node_atom(parser, parser->i_current_token);
		AstNode *new_node = vec_at(&parser->nodes, new_node_index);
		if (new_node_index < parser->nodes.length) {
			new_node->span = current_token.span;
			parser->i_current_token += 1;
		}
		return new_node_index;
	} else {
		parser->result = ParserResult::UnexpectedToken;
		parser->error = current_token.span;
		parser->expected_token_kind = TokenKind::Number;
		return parser->nodes.length;
	}
}

// sexpr | atom
static uint32_t parse_expression(Parser *parser);

// () | (identifier exp*)
static uint32_t parse_s_expression(Parser *parser)
{
	auto left_paren_token = parser_expect_token(parser, TokenKind::LeftParen);
	(void)(left_paren_token);

	Token peek_token = parser_current_token(parser);
	// 0  - Empty list
	if (peek_token.kind == TokenKind::RightParen) {
		parser->i_current_token += 1;
		return parser_push_ast_node_sexpr(parser, INVALID_NODE_INDEX);
	}

	// 1  - First symbol is something to evaluate (function, variable or builtin)
	uint32_t first_expr_node_index = parse_expression(parser);
	uint32_t sexpr_node_index = parser_push_ast_node_sexpr(parser, first_expr_node_index);

	// 2+ - arguments to evaluate the first symbol
	peek_token = parser_current_token(parser);
	AstNode *current_child = vec_at(&parser->nodes, first_expr_node_index);
	while (peek_token.kind != TokenKind::RightParen && peek_token.kind != TokenKind::Invalid &&
		   parser->result == ParserResult::Ok) {
		uint32_t new_expr_node_index = parse_expression(parser);
		ast_node_add_sibling(current_child, new_expr_node_index);
		
		current_child = vec_at(&parser->nodes, new_expr_node_index);
		peek_token = parser_current_token(parser);
	}

	auto right_paren_token = parser_expect_token(parser, TokenKind::RightParen);

	AstNode *sexpr_node = vec_at(&parser->nodes, sexpr_node_index);
	sexpr_node->span = span_extend(left_paren_token.span, right_paren_token.span);

	return sexpr_node_index;
}

// sexpr | atom
static uint32_t parse_expression(Parser *parser)
{
	Token i_current_token = parser_current_token(parser);
	if (i_current_token.kind == TokenKind::LeftParen) {
		return parse_s_expression(parser);
	} else {
		return parse_atom(parser);
	}
}

// sexpr*
vec<AstNode> parse_module(Parser *parser)
{
	uint32_t root_node_index = parser_push_ast_node_sexpr(parser, INVALID_NODE_INDEX);
	AstNode *root_node = vec_at(&parser->nodes, root_node_index);

	AstNode *current_child = nullptr;
	while (parser->i_current_token < parser->tokens.length) {
		uint32_t expr_node_index = parse_s_expression(parser);
		if (current_child == nullptr) {
			root_node->left_child_index = expr_node_index;
		} else {
			current_child->right_sibling_index = expr_node_index;
		}
		current_child = vec_at(&parser->nodes, expr_node_index);
		if (parser->result != ParserResult::Ok)
			break;
	}

	return parser->nodes;
}

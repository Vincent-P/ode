#include "parser.h"
#include "lexer.h"
#include "compiler.h"
#include <stdio.h>

// LISP parsing

static Token parser_current_token(Parser *parser)
{
	if (parser->i_current_token < parser->compunit->tokens.length) {
		return *vec_at(&parser->compunit->tokens, parser->i_current_token);
	} else {
		parser->compunit->error.code = ErrorCode::ExpectedTokenGotEof;
		parser->compunit->error.span = {};
		if (parser->compunit->tokens.length > 0) {
			const Token *last_token = vec_at(&parser->compunit->tokens, parser->compunit->tokens.length - 1);
			parser->compunit->error.span = last_token->span;
		}
		return {};
	}
}

static Token parser_expect_token(Parser *parser, TokenKind expect_kind)
{
	if (parser->compunit->error.code != ErrorCode::Ok) {
		return {};
	}

	if (parser->i_current_token >= parser->compunit->tokens.length) {
		parser->compunit->error.code = ErrorCode::ExpectedTokenGotEof;
		parser->compunit->error.span = {};
		if (parser->compunit->tokens.length > 0) {
			const Token *last_token = vec_at(&parser->compunit->tokens, parser->compunit->tokens.length - 1);
			parser->compunit->error.span = last_token->span;
		}
		return {};
	}

	Token token = *vec_at(&parser->compunit->tokens, parser->i_current_token);

	if (token.kind == expect_kind) {
		parser->i_current_token += 1;
		parser->compunit->error.code = ErrorCode::Ok;
	} else {
		parser->compunit->error.code = ErrorCode::UnexpectedToken;
		parser->compunit->error.span = token.span;
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
	return vec_append(&parser->compunit->nodes, new_node);
}

static uint32_t parser_push_ast_node_sexpr(Parser *parser, uint32_t first_child_index)
{
	AstNode new_node = {};
	new_node.atom_token_index = INVALID_NODE_INDEX;
	new_node.right_sibling_index = INVALID_NODE_INDEX;
	new_node.left_child_index = first_child_index;
	if (new_node.left_child_index != INVALID_NODE_INDEX) {
		new_node.child_count = 1;
	}
	return vec_append(&parser->compunit->nodes, new_node);
}

// number | identifier
static uint32_t parse_atom(Parser *parser)
{
	Token current_token = parser_current_token(parser);

	bool is_a_valid_atom = current_token.kind == TokenKind::Number || current_token.kind == TokenKind::Identifier
	                       || current_token.kind == TokenKind::StringLiteral;
	if (is_a_valid_atom) {
		uint32_t new_node_index = parser_push_ast_node_atom(parser, parser->i_current_token);
		AstNode *new_node = vec_at(&parser->compunit->nodes, new_node_index);
		if (new_node_index < parser->compunit->nodes.length) {
			new_node->span = current_token.span;
			parser->i_current_token += 1;
		}
		return new_node_index;
	} else {
		parser->compunit->error.code = ErrorCode::UnexpectedToken;
		parser->compunit->error.span = current_token.span;
		parser->expected_token_kind = TokenKind::Number;
		return parser->compunit->nodes.length;
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
	const uint32_t first_expr_node_index = parse_expression(parser);
	const uint32_t sexpr_node_index = parser_push_ast_node_sexpr(parser, first_expr_node_index);

	// 2+ - arguments to evaluate the first symbol
	peek_token = parser_current_token(parser);
	AstNode *current_child = vec_at(&parser->compunit->nodes, first_expr_node_index);
	while (peek_token.kind != TokenKind::RightParen && peek_token.kind != TokenKind::Invalid
		   && parser->compunit->error.code == ErrorCode::Ok) {
		const uint32_t new_expr_node_index = parse_expression(parser);
		current_child->right_sibling_index = new_expr_node_index;

		AstNode *const sexpr_node = vec_at(&parser->compunit->nodes, sexpr_node_index);
		sexpr_node->child_count += 1;

		current_child = vec_at(&parser->compunit->nodes, new_expr_node_index);
		peek_token = parser_current_token(parser);
	}

	auto right_paren_token = parser_expect_token(parser, TokenKind::RightParen);

	AstNode *sexpr_node = vec_at(&parser->compunit->nodes, sexpr_node_index);
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
	AstNode *root_node = vec_at(&parser->compunit->nodes, root_node_index);

	AstNode *current_child = nullptr;
	while (parser->i_current_token < parser->compunit->tokens.length) {
		uint32_t expr_node_index = parse_s_expression(parser);
		root_node->child_count += 1;
		if (current_child == nullptr) {
			root_node->left_child_index = expr_node_index;
		} else {
			current_child->right_sibling_index = expr_node_index;
		}
		current_child = vec_at(&parser->compunit->nodes, expr_node_index);
		if (parser->compunit->error.code != ErrorCode::Ok)
			break;
	}

	return parser->compunit->nodes;
}


// Typed nodes parsing
TypeID parse_type(CompilationUnit* compunit, Module *module, const AstNode *node)
{
	if (ast_is_atom(node)) {
		// The type is just an identifier, find the corresponding builtin type or named type
		const Token *identifier = ast_get_token(&compunit->tokens, node);
		sv identifier_str = sv_substr(compunit->input, identifier->span);

		// Search builtin types
		for (uint32_t i_builtin_type = 0; i_builtin_type < ARRAY_LENGTH(BuiltinTypeKind_str); ++i_builtin_type) {
			const char *builtin_str = BuiltinTypeKind_str[i_builtin_type];
			if (sv_equals(identifier_str, sv_from_null_terminated(builtin_str))) {
				return type_id_new_builtin(BuiltinTypeKind(i_builtin_type));
			}
		}

		// Search named types
		for (uint32_t i_type = 0; i_type < module->types_length; ++i_type) {
			if (sv_equals(module->types[i_type].name, identifier_str)) {
				return type_id_new_user_defined(i_type);
			}
		}

		// We haven't found any named type
		INIT_ERROR(&compunit->error, ErrorCode::UnknownSymbol); 
		compunit->error.span = node->span;
		return UNIT_TYPE;
	} else {
		// The compiler is a S-expression
		if (!ast_has_left_child(node)) {
			// () is a unit type
			return UNIT_TYPE;
		}

		const AstNode *child0 = ast_get_left_child(&compunit->nodes, node);
		// The type is a S-expr, either it is a unit type () or a form starting with a TOKEN
		if (!ast_is_atom(child0)) {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedIdentifier);
			compunit->error.span = child0->span;
			return UNIT_TYPE;
		}
		// Assert that there is at least one argument
		if (!ast_has_right_sibling(child0)) {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
			compunit->error.span = node->span;
			return UNIT_TYPE;
		}
		const AstNode *child1 = ast_get_right_sibling(&compunit->nodes, child0);
		// Assert that there is only one argument
		if (ast_has_right_sibling(child1)) {
			const AstNode *child2 = ast_get_right_sibling(&compunit->nodes, child1);
			INIT_ERROR(&compunit->error, ErrorCode::UnexpectedExpression);
			compunit->error.span = child2->span;
			return UNIT_TYPE;
		}

		const Token *token = ast_get_token(&compunit->tokens, child0);
		const sv token_str = sv_substr(compunit->input, token->span);

		// We only support type S-expr of the forms:
		// (* <type>)
		if (sv_equals(token_str, sv_from_null_terminated("*"))) {
			TypeID inner_type = parse_type(compunit, module, child1);
			return type_id_pointer_from(inner_type);
		}

		// We haven't found any builtin
		INIT_ERROR(&compunit->error, ErrorCode::UnknownSymbol);
		compunit->error.span = token->span;
		return UNIT_TYPE;
	}
}

// Parse a define function signature without a body
// (define (<name> <return_type>) (<args>*) <expression>+)
void parse_define_sig(CompilationUnit* compunit, const AstNode *node, DefineNode *output)
{
	if (node->child_count < 3) {
		INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}

	const AstNode *define_token_node = ast_get_left_child(&compunit->nodes, node);
	const AstNode *name_type_node = ast_get_right_sibling(&compunit->nodes, define_token_node);
	const AstNode *arglist_node = ast_get_right_sibling(&compunit->nodes, name_type_node);

	// Get the function name node
	if (name_type_node->child_count != 2) {
		INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}

	const AstNode *function_name_node = ast_get_left_child(&compunit->nodes, name_type_node);
	if (!ast_is_atom(function_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode::ExpectedIdentifier);
		compunit->error.span = node->span;
		return;
	}

	output->function_name_token = ast_get_token(&compunit->tokens, function_name_node);
	output->return_type_node = ast_get_right_sibling(&compunit->nodes, function_name_node);
	output->arglist_node = arglist_node;

	// Arguments are laid out as (name1 type1 name2 type2 ...)
	uint32_t i_arg_node = arglist_node->left_child_index;
	uint32_t args_length = 0;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_name_node = ast_get_node(&compunit->nodes, i_arg_node);
		const Token *arg_name = ast_get_token(&compunit->tokens, arg_name_node);
		const bool arg_is_an_identifier = ast_is_atom(arg_name_node) && arg_name->kind == TokenKind::Identifier;
		if (!arg_is_an_identifier) {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedIdentifier);
			compunit->error.span = arglist_node->span;
			return;
		}

		const AstNode *arg_type_node = ast_get_right_sibling(&compunit->nodes, arg_name_node);
		if (!ast_has_right_sibling(arg_name_node)) {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
			compunit->error.span = arglist_node->span;
			return;
		}

		output->arg_identifiers[args_length] = *arg_name;
		output->arg_nodes[args_length] = arg_type_node;
		args_length += 1;
		i_arg_node = arg_type_node->right_sibling_index;
	}
	output->args_length = args_length;
}

void parse_define_body(CompilationUnit* compunit, const AstNode *node, DefineNode *output)
{
	if (!ast_has_right_sibling(output->arglist_node)) {
		INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}
	output->body_node = ast_get_right_sibling(&compunit->nodes, output->arglist_node);
}

// (struct <name> (<field name> <field type>)+)
void parse_struct(CompilationUnit *compunit, const AstNode *node, StructNode *output)
{
	const AstNode *struct_token_node = ast_get_left_child(&compunit->nodes, node);

	// Get struct name node
	const AstNode *struct_name_node = ast_get_right_sibling(&compunit->nodes, struct_token_node);
	if (!ast_has_right_sibling(struct_token_node) || !ast_is_atom(struct_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode::ExpectedIdentifier);
		compunit->error.span = node->span;
		return;
	}
	output->struct_name_token = ast_get_token(&compunit->tokens, struct_name_node);

	// Get fields
	// Get the first field (a struct MUST have at least one field)
	if (!ast_has_right_sibling(struct_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}
	uint32_t fields_length = 0;
	uint32_t i_field_node = struct_name_node->right_sibling_index;
	while (ast_is_valid(i_field_node)) {
		const AstNode *field_node = ast_get_node(&compunit->nodes, i_field_node);
		// Get the field name
		const AstNode *field_identifier_node = ast_get_left_child(&compunit->nodes, field_node);
		if (!ast_has_left_child(field_node)) {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedIdentifier);
			compunit->error.span = field_node->span;
			return;
		}
		const Token *field_identifier_token = ast_get_token(&compunit->tokens, field_identifier_node);
		const bool is_an_identifier_token_node =
			ast_is_atom(field_identifier_node) && field_identifier_token->kind == TokenKind::Identifier;
		if (!is_an_identifier_token_node) {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedIdentifier);
			compunit->error.span = field_identifier_node->span;
			return;
		}
		// Get the field type
		if (!ast_has_right_sibling(field_identifier_node)) {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
			compunit->error.span = field_node->span;
			return;
		}
		const AstNode *field_type_node = ast_get_right_sibling(&compunit->nodes, field_identifier_node);
		if (fields_length > MAX_STRUCT_FIELDS) {
			INIT_ERROR(&compunit->error, ErrorCode::Fatal);
			compunit->error.span = field_identifier_node->span;
			return;
		}

		output->field_identifiers[fields_length] = *field_identifier_token;
		output->field_type_nodes[fields_length] = field_type_node;
		fields_length += 1;

		i_field_node = field_node->right_sibling_index;
	}
	output->fields_length = fields_length;
}

// (if <cond_expression> <then_expression> <else_expression>)
void parse_if(CompilationUnit *compunit, const AstNode *node, IfNode *output)
{
	if (node->child_count != 4) {
		if (node->child_count > 4) {
			INIT_ERROR(&compunit->error, ErrorCode::UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *if_token_node = ast_get_left_child(&compunit->nodes, node);
	output->cond_expr_node = ast_get_right_sibling(&compunit->nodes, if_token_node);
	output->then_expr_node = ast_get_right_sibling(&compunit->nodes, output->cond_expr_node);
	output->else_expr_node = ast_get_right_sibling(&compunit->nodes, output->then_expr_node);
}

// (let <name> <value expr>)
void parse_let(CompilationUnit *compunit, const AstNode *node, LetNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			INIT_ERROR(&compunit->error, ErrorCode::UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *let_token_node = ast_get_left_child(&compunit->nodes, node);
	const AstNode *name_node = ast_get_right_sibling(&compunit->nodes, let_token_node);
	output->name_token = ast_get_token(&compunit->tokens, name_node);
	output->value_node = ast_get_right_sibling(&compunit->nodes, name_node);
}

void parse_binary_op(CompilationUnit *compunit, const AstNode *node, BinaryOpNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			INIT_ERROR(&compunit->error, ErrorCode::UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}
	const AstNode *op_node = ast_get_left_child(&compunit->nodes, node);
	const AstNode *lhs_node = ast_get_right_sibling(&compunit->nodes, op_node);
	const AstNode *rhs_node = ast_get_right_sibling(&compunit->nodes, lhs_node);
	output->lhs_node = lhs_node;
	output->rhs_node = rhs_node;
}

void parse_unary_op(CompilationUnit *compunit, const AstNode *node, UnaryOpNode *output)
{
	if (node->child_count != 2) {
		if (node->child_count > 2) {
			INIT_ERROR(&compunit->error, ErrorCode::UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *op_node = ast_get_left_child(&compunit->nodes, node);
	const AstNode *value_node = ast_get_right_sibling(&compunit->nodes, op_node);
	output->value_node = value_node;
}

// (field <expr> <member identifier>)
void parse_field(CompilationUnit *compunit, const AstNode *node, FieldNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			INIT_ERROR(&compunit->error, ErrorCode::UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *field_token_node = ast_get_left_child(&compunit->nodes, node);
	const AstNode *expr_node = ast_get_right_sibling(&compunit->nodes, field_token_node);
	const AstNode *field_identifier_node = ast_get_right_sibling(&compunit->nodes, expr_node);

	const Token *field_identifier_token = ast_get_token(&compunit->tokens, field_identifier_node);
	if (!ast_is_atom(field_identifier_node) || field_identifier_token->kind != TokenKind::Identifier) {
		INIT_ERROR(&compunit->error, ErrorCode::ExpectedIdentifier);
		compunit->error.span = field_identifier_node->span;
		return;
	}

	output->expr_node = expr_node;
	output->field_token = field_identifier_token;
}

// (_ptr_offset <type> <base pointer> <offset>)
void parse_ptr_offset(CompilationUnit* compunit, const AstNode *node, PtrOffsetNodes *output)
{
	if (node->child_count != 4) {
		if (node->child_count > 4) {
			INIT_ERROR(&compunit->error, ErrorCode::UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode::ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *ptr_offset_token_node = ast_get_left_child(&compunit->nodes, node);
	const AstNode *return_type_node = ast_get_right_sibling(&compunit->nodes, ptr_offset_token_node);
	const AstNode *base_pointer_node = ast_get_right_sibling(&compunit->nodes, return_type_node);
	const AstNode *offset_node = ast_get_right_sibling(&compunit->nodes, base_pointer_node);

	output->return_type_node = return_type_node;
	output->base_pointer_node = base_pointer_node;
	output->offset_node = offset_node;
}

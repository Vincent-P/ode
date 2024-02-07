#include "parser.h"
#include "compiler.h"
#include "lexer.h"

// Ast nodes

const AstNode *ast_get_node(const CompilationUnit *compunit, uint32_t node_index)
{
	if (node_index >= compunit->nodes_length) {
		__debugbreak();
	}
	return compunit->nodes + node_index;
}
	
const AstNode *ast_get_left_child(const CompilationUnit *compunit, const AstNode *node)
{
	return ast_get_node(compunit, node->left_child_index);
}
	
const AstNode *ast_get_right_sibling(const CompilationUnit *compunit, const AstNode *node)
{
	return ast_get_node(compunit, node->right_sibling_index);
}
	
const Token *ast_get_token(const CompilationUnit *compunit, const AstNode *node)
{
	uint32_t token_index = node->atom_token_index;
	if (token_index >= compunit->tokens_length) {
		__debugbreak();
	}
	return compunit->tokens + token_index;
}


// LISP parsing

static Token parser_current_token(Parser *parser)
{
	if (parser->i_current_token < parser->compunit->tokens_length) {
		return parser->compunit->tokens[parser->i_current_token];
	} else {
		parser->compunit->error.code = ErrorCode_ExpectedTokenGotEof;
		parser->compunit->error.span = (span){0};
		if (parser->compunit->tokens_length > 0) {
			const Token *last_token = parser->compunit->tokens + (parser->compunit->tokens_length - 1);
			parser->compunit->error.span = last_token->span;
		}
		return (Token){0};
	}
}

static Token parser_expect_token(Parser *parser, TokenKind expect_kind)
{
	if (parser->compunit->error.code != ErrorCode_Ok) {
		return (Token){0};
	}

	if (parser->i_current_token >= parser->compunit->tokens_length) {
		parser->compunit->error.code = ErrorCode_ExpectedTokenGotEof;
		parser->compunit->error.span = (span){0};
		if (parser->compunit->tokens_length > 0) {
			const Token *last_token = parser->compunit->tokens + (parser->compunit->tokens_length - 1);
			parser->compunit->error.span = last_token->span;
		}
		return (Token){0};
	}

	Token token = parser->compunit->tokens[parser->i_current_token];

	if (token.kind == expect_kind) {
		parser->i_current_token += 1;
		parser->compunit->error.code = ErrorCode_Ok;
	} else {
		INIT_ERROR(&parser->compunit->error, ErrorCode_UnexpectedToken);
		parser->compunit->error.span = token.span;
		parser->expected_token_kind = expect_kind;
	}
	return token;
}

static uint32_t parser_push_ast_node_atom(Parser *parser, uint32_t token_index)
{
	AstNode new_node = {0};
	new_node.atom_token_index = token_index;
	new_node.left_child_index = INVALID_NODE_INDEX;
	new_node.right_sibling_index = INVALID_NODE_INDEX;

	uint32_t i_node = parser->compunit->nodes_length;
	parser->compunit->nodes[i_node] = new_node;
	parser->compunit->nodes_length += 1;
	return i_node;
}

static uint32_t parser_push_ast_node_sexpr(Parser *parser, uint32_t first_child_index)
{
	AstNode new_node = {0};
	new_node.atom_token_index = INVALID_NODE_INDEX;
	new_node.right_sibling_index = INVALID_NODE_INDEX;
	new_node.left_child_index = first_child_index;
	if (new_node.left_child_index != INVALID_NODE_INDEX) {
		new_node.child_count = 1;
	}
	uint32_t i_node = parser->compunit->nodes_length;
	parser->compunit->nodes[i_node] = new_node;
	parser->compunit->nodes_length += 1;
	return i_node;
}

// number | identifier
static uint32_t parse_atom(Parser *parser)
{
	Token current_token = parser_current_token(parser);

	bool is_a_valid_atom = current_token.kind == TokenKind_UnsignedNumber
			       || current_token.kind == TokenKind_SignedNumber
			       || current_token.kind == TokenKind_FloatingNumber
			       || current_token.kind == TokenKind_Identifier
	                       || current_token.kind == TokenKind_StringLiteral;
	if (is_a_valid_atom) {
		uint32_t new_node_index = parser_push_ast_node_atom(parser, parser->i_current_token);
		AstNode *new_node = parser->compunit->nodes + new_node_index;
		if (new_node_index < parser->compunit->nodes_length) {
			new_node->span = current_token.span;
			parser->i_current_token += 1;
		}
		return new_node_index;
	} else {
		INIT_ERROR(&parser->compunit->error, ErrorCode_UnexpectedToken);
		parser->compunit->error.span = current_token.span;
		parser->expected_token_kind = TokenKind_UnsignedNumber;
		return parser->compunit->nodes_length;
	}
}

// sexpr | atom
static uint32_t parse_expression(Parser *parser);

// () | (identifier exp*)
static uint32_t parse_s_expression(Parser *parser)
{
	Token left_paren_token = parser_expect_token(parser, TokenKind_LeftParen);
	(void)(left_paren_token);

	Token peek_token = parser_current_token(parser);
	// 0  - Empty list
	if (peek_token.kind == TokenKind_RightParen) {
		parser->i_current_token += 1;
		return parser_push_ast_node_sexpr(parser, INVALID_NODE_INDEX);
	}

	// 1  - First symbol is something to evaluate (function, variable or builtin)
	const uint32_t first_expr_node_index = parse_expression(parser);
	const uint32_t sexpr_node_index = parser_push_ast_node_sexpr(parser, first_expr_node_index);

	// 2+ - arguments to evaluate the first symbol
	peek_token = parser_current_token(parser);
	AstNode *current_child = parser->compunit->nodes + first_expr_node_index;
	while (peek_token.kind != TokenKind_RightParen && peek_token.kind != TokenKind_Invalid
		   && parser->compunit->error.code == ErrorCode_Ok) {
		const uint32_t new_expr_node_index = parse_expression(parser);
		current_child->right_sibling_index = new_expr_node_index;

		AstNode *const sexpr_node = parser->compunit->nodes + sexpr_node_index;
		sexpr_node->child_count += 1;

		current_child = parser->compunit->nodes + new_expr_node_index;
		peek_token = parser_current_token(parser);
	}

	Token right_paren_token = parser_expect_token(parser, TokenKind_RightParen);

	AstNode *sexpr_node = parser->compunit->nodes + sexpr_node_index;
	sexpr_node->span = span_extend(left_paren_token.span, right_paren_token.span);

	return sexpr_node_index;
}

// sexpr | atom
static uint32_t parse_expression(Parser *parser)
{
	Token i_current_token = parser_current_token(parser);
	if (i_current_token.kind == TokenKind_LeftParen) {
		return parse_s_expression(parser);
	} else {
		return parse_atom(parser);
	}
}

// sexpr*
void parse_module(Parser *parser)
{
	uint32_t root_node_index = parser_push_ast_node_sexpr(parser, INVALID_NODE_INDEX);
	AstNode *root_node = parser->compunit->nodes + root_node_index;

	AstNode *current_child = NULL;
	while (parser->i_current_token < parser->compunit->tokens_length) {
		uint32_t expr_node_index = parse_s_expression(parser);
		root_node->child_count += 1;
		if (current_child == NULL) {
			root_node->left_child_index = expr_node_index;
		} else {
			current_child->right_sibling_index = expr_node_index;
		}
		current_child = parser->compunit->nodes + expr_node_index;
		if (parser->compunit->error.code != ErrorCode_Ok)
			break;
	}
}

// Typed nodes parsing
TypeID parse_type(CompilationUnit *compunit, CompilerModule *module, const AstNode *node)
{
	if (ast_is_atom(node)) {
		// The type is just an identifier, find the corresponding builtin type or named type
		const Token *identifier = ast_get_token(compunit, node);
		sv identifier_str = sv_substr(compunit->input, identifier->span);

		// Search builtin types
		const TypeID builtin_types[] = {
			type_id_new_builtin(BuiltinTypeKind_Unit),
			type_id_new_signed(NumberWidth_8),
			type_id_new_signed(NumberWidth_16),
			type_id_new_signed(NumberWidth_32),
			type_id_new_signed(NumberWidth_64),
			type_id_new_unsigned(NumberWidth_8),
			type_id_new_unsigned(NumberWidth_16),
			type_id_new_unsigned(NumberWidth_32),
			type_id_new_unsigned(NumberWidth_64),
			type_id_new_builtin(BuiltinTypeKind_Bool),
			type_id_new_builtin(BuiltinTypeKind_Float),
		};
		char builtin_type_name_buf[32] = {0};
		StringBuilder sb;
		for (uint32_t i_builtin_type = 0; i_builtin_type < ARRAY_LENGTH(builtin_types); ++i_builtin_type) {
			TypeID type_id = builtin_types[i_builtin_type];
			sb = string_builder_from_buffer(builtin_type_name_buf, sizeof(builtin_type_name_buf));
			type_build_string(&sb, type_id);
			const sv builtin_str = string_builder_get_string(&sb);
			if (sv_equals(identifier_str, builtin_str)) {
				return type_id;
			}
		}

		// Search named types
		for (uint32_t i_type = 0; i_type < module->types_length; ++i_type) {
			if (sv_equals(module->types[i_type].name, identifier_str)) {
				return type_id_new_user_defined(i_type);
			}
		}

		// We haven't found any named type
		INIT_ERROR(&compunit->error, ErrorCode_UnknownSymbol);
		compunit->error.span = node->span;
		return UNIT_TYPE;
	} else {
		// The compiler is a S-expression
		if (!ast_has_left_child(node)) {
			// () is a unit type
			return UNIT_TYPE;
		}

		const AstNode *child0 = ast_get_left_child(compunit, node);
		// The type is a S-expr, either it is a unit type () or a form starting with a TOKEN
		if (!ast_is_atom(child0)) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			compunit->error.span = child0->span;
			return UNIT_TYPE;
		}
		// Assert that there is at least one argument
		if (!ast_has_right_sibling(child0)) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
			compunit->error.span = node->span;
			return UNIT_TYPE;
		}
		const AstNode *child1 = ast_get_right_sibling(compunit, child0);
		// Assert that there is only one argument
		if (ast_has_right_sibling(child1)) {
			const AstNode *child2 = ast_get_right_sibling(compunit, child1);
			INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
			compunit->error.span = child2->span;
			return UNIT_TYPE;
		}

		const Token *token = ast_get_token(compunit, child0);
		const sv token_str = sv_substr(compunit->input, token->span);
		// (* <type>)
		if (sv_equals(token_str, sv_from_null_terminated("*"))) {
			TypeID inner_type = parse_type(compunit, module, child1);
			return type_id_pointer_from(inner_type);
		}
		else if (sv_equals(token_str, sv_from_null_terminated("[]"))) {
			TypeID inner_type = parse_type(compunit, module, child1);
			TypeID slice_type = type_id_pointer_from(inner_type);
			slice_type.slice.builtin_kind = BuiltinTypeKind_Slice;
			return slice_type;
		}

		// We haven't found any builtin
		INIT_ERROR(&compunit->error, ErrorCode_UnknownSymbol);
		compunit->error.span = token->span;
		return UNIT_TYPE;
	}
}

// Parse a define function signature without a body
// (define (<name> <return_type>) (<args>*) <expression>+)
void parse_define_sig(CompilationUnit *compunit, const AstNode *node, DefineNode *output)
{
	if (node->child_count < 3) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}

	const AstNode *define_token_node = ast_get_left_child(compunit, node);
	const AstNode *name_type_node = ast_get_right_sibling(compunit, define_token_node);
	const AstNode *arglist_node = ast_get_right_sibling(compunit, name_type_node);

	// Get the function name node
	if (name_type_node->child_count != 2) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}

	const AstNode *function_name_node = ast_get_left_child(compunit, name_type_node);
	if (!ast_is_atom(function_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
		compunit->error.span = node->span;
		return;
	}

	output->function_name_token = ast_get_token(compunit, function_name_node);
	output->return_type_node = ast_get_right_sibling(compunit, function_name_node);
	output->arglist_node = arglist_node;

	// Arguments are laid out as (name1 type1 name2 type2 ...)
	uint32_t i_arg_node = arglist_node->left_child_index;
	uint32_t args_length = 0;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_name_node = ast_get_node(compunit, i_arg_node);
		const Token *arg_name = ast_get_token(compunit, arg_name_node);
		const bool arg_is_an_identifier = ast_is_atom(arg_name_node) && arg_name->kind == TokenKind_Identifier;
		if (!arg_is_an_identifier) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			compunit->error.span = arglist_node->span;
			return;
		}

		const AstNode *arg_type_node = ast_get_right_sibling(compunit, arg_name_node);
		if (!ast_has_right_sibling(arg_name_node)) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
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

void parse_define_body(CompilationUnit *compunit, const AstNode *node, DefineNode *output)
{
	if (!ast_has_right_sibling(output->arglist_node)) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}
	output->body_node = ast_get_right_sibling(compunit, output->arglist_node);
}

// (struct <name> (<field name> <field type>)+)
void parse_struct(CompilationUnit *compunit, const AstNode *node, StructNode *output)
{
	const AstNode *struct_token_node = ast_get_left_child(compunit, node);

	// Get struct name node
	const AstNode *struct_name_node = ast_get_right_sibling(compunit, struct_token_node);
	if (!ast_has_right_sibling(struct_token_node) || !ast_is_atom(struct_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
		compunit->error.span = node->span;
		return;
	}
	output->struct_name_token = ast_get_token(compunit, struct_name_node);

	// Get fields
	// Get the first field (a struct MUST have at least one field)
	if (!ast_has_right_sibling(struct_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}
	uint32_t fields_length = 0;
	uint32_t i_field_node = struct_name_node->right_sibling_index;
	while (ast_is_valid(i_field_node)) {
		const AstNode *field_node = ast_get_node(compunit, i_field_node);
		// Get the field name
		const AstNode *field_identifier_node = ast_get_left_child(compunit, field_node);
		if (!ast_has_left_child(field_node)) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			compunit->error.span = field_node->span;
			return;
		}
		const Token *field_identifier_token = ast_get_token(compunit, field_identifier_node);
		const bool is_an_identifier_token_node =
			ast_is_atom(field_identifier_node) && field_identifier_token->kind == TokenKind_Identifier;
		if (!is_an_identifier_token_node) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			compunit->error.span = field_identifier_node->span;
			return;
		}
		// Get the field type
		if (!ast_has_right_sibling(field_identifier_node)) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
			compunit->error.span = field_node->span;
			return;
		}
		const AstNode *field_type_node = ast_get_right_sibling(compunit, field_identifier_node);
		if (fields_length > MAX_STRUCT_FIELDS) {
			INIT_ERROR(&compunit->error, ErrorCode_Fatal);
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
			INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *if_token_node = ast_get_left_child(compunit, node);
	output->cond_expr_node = ast_get_right_sibling(compunit, if_token_node);
	output->then_expr_node = ast_get_right_sibling(compunit, output->cond_expr_node);
	output->else_expr_node = ast_get_right_sibling(compunit, output->then_expr_node);
}

// (let <name> <value expr>)
void parse_let(CompilationUnit *compunit, const AstNode *node, LetNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *let_token_node = ast_get_left_child(compunit, node);
	const AstNode *name_node = ast_get_right_sibling(compunit, let_token_node);
	output->name_token = ast_get_token(compunit, name_node);
	output->value_node = ast_get_right_sibling(compunit, name_node);
}

// (field <expr> <member identifier>)
void parse_field(CompilationUnit *compunit, const AstNode *node, FieldNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *field_token_node = ast_get_left_child(compunit, node);
	const AstNode *expr_node = ast_get_right_sibling(compunit, field_token_node);
	const AstNode *field_identifier_node = ast_get_right_sibling(compunit, expr_node);

	const Token *field_identifier_token = ast_get_token(compunit, field_identifier_node);
	if (!ast_is_atom(field_identifier_node) || field_identifier_token->kind != TokenKind_Identifier) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
		compunit->error.span = field_identifier_node->span;
		return;
	}

	output->expr_node = expr_node;
	output->field_token = field_identifier_token;
}

// (_ptr_offset <type> <base pointer> <offset>)
void parse_ptr_offset(CompilationUnit *compunit, const AstNode *node, PtrOffsetNodes *output)
{
	if (node->child_count != 4) {
		if (node->child_count > 4) {
			INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}

	const AstNode *ptr_offset_token_node = ast_get_left_child(compunit, node);
	const AstNode *return_type_node = ast_get_right_sibling(compunit, ptr_offset_token_node);
	const AstNode *base_pointer_node = ast_get_right_sibling(compunit, return_type_node);
	const AstNode *offset_node = ast_get_right_sibling(compunit, base_pointer_node);

	output->return_type_node = return_type_node;
	output->base_pointer_node = base_pointer_node;
	output->offset_node = offset_node;
}

 // (identifier arg0 arg1 .. argn)
void parse_nary_op(CompilationUnit *compunit, const AstNode *node, uint32_t n, const AstNode **out_nodes)
{
	uint32_t expected_child_count = n + 1;
	if (node->child_count != expected_child_count) {
		if (node->child_count > expected_child_count) {
			INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
		} else {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		}
		compunit->error.span = node->span;
		return;
	}
	
	const AstNode *current_node = ast_get_left_child(compunit, node);
	for (uint32_t i = 0; i < n; ++i) {
		current_node = ast_get_right_sibling(compunit, current_node);
		out_nodes[i] = current_node;
	}
}

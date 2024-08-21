#include "parser.h"
#include "compiler.h"
#include "lexer.h"

struct ParserInput
{
	AstNode *nodes;
	const Token *tokens;
};
typedef struct ParserInput ParserInput;

struct ParserContext
{
	uint32_t i_current_token;
	Token current_token;
	Arena *memory;
};
typedef struct ParserContext ParserContext;

struct NodeResult
{
	AstNode *node;
	uint32_t node_id;
	bool ok;
};
typedef struct NodeResult NodeResult;

// LISP parsing
static void parser_eat_token(ParserInput input, ParserContext *context)
{
	context->i_current_token += 1;
	context->current_token = array_check(input.tokens, context->i_current_token);
}

static NodeResult parser_push_ast_node_atom(ParserInput input, ParserContext *context, uint32_t token_index)
{
	AstNode new_node = (AstNode){0};
	new_node.atom_token_index = token_index;
	new_node.left_child_index = INVALID_NODE_INDEX;
	new_node.right_sibling_index = INVALID_NODE_INDEX;

	NodeResult result = (NodeResult){0};
	result.node_id = array_count(input.nodes);
	result.node = array_push_addr(context->memory, input.nodes, new_node);
	result.ok = true;
	return result;
}

static NodeResult parser_push_ast_node_sexpr(ParserInput input, ParserContext *context, uint32_t first_child_index)
{
	AstNode new_node = (AstNode){0};
	new_node.atom_token_index = INVALID_NODE_INDEX;
	new_node.right_sibling_index = INVALID_NODE_INDEX;
	new_node.left_child_index = first_child_index;
	if (new_node.left_child_index != INVALID_NODE_INDEX) {
		new_node.child_count = 1;
	}

	NodeResult result = (NodeResult){0};
	result.node_id = array_count(input.nodes);
	result.node = array_push_addr(context->memory, input.nodes, new_node);
	result.ok = true;
	return result;
}

// number | identifier
static NodeResult parse_atom(ParserInput input, ParserContext *context)
{
	NodeResult result = (NodeResult){0};
	Token current_token = context->current_token;

	bool is_a_valid_atom = current_token.kind == TokenKind_UnsignedNumber
			       || current_token.kind == TokenKind_SignedNumber
			       || current_token.kind == TokenKind_FloatingNumber
			       || current_token.kind == TokenKind_Identifier
	                       || current_token.kind == TokenKind_StringLiteral;
	if (!is_a_valid_atom) {
		return result;
#if 0
		INIT_ERROR(&parser->compunit->error, ErrorCode_UnexpectedToken);
		parser->compunit->error.span = current_token.span;
		parser->expected_token_kind = TokenKind_UnsignedNumber;
		return parser->compunit->nodes_length;
#endif
	}

	result = parser_push_ast_node_atom(input, context, context->i_current_token);
	result.node->span = current_token.span;
	parser_eat_token(input, context);
	return result;
}

// sexpr | atom
static NodeResult parse_expression(ParserInput input, ParserContext *context);

// () | (identifier exp*)
static NodeResult parse_s_expression(ParserInput input, ParserContext *context)
{
	if (context->current_token.kind != TokenKind_LeftParen) {
		return (NodeResult){0};
	}
	Token left_paren_token = context->current_token;
	parser_eat_token(input, context); // eat left paren

	// 0  - Empty list
	if (context->current_token.kind == TokenKind_RightParen) {
		parser_eat_token(input, context);
		return parser_push_ast_node_sexpr(input, context, INVALID_NODE_INDEX);
	}

	// 1  - First symbol is something to evaluate (function, variable or builtin)
	NodeResult first_expr_node = parse_expression(input, context);
	NodeResult result = parser_push_ast_node_sexpr(input, context, first_expr_node.node_id);

	// 2+ - arguments to evaluate the first symbol
	AstNode *current_child = first_expr_node.node;
	while (context->current_token.kind != TokenKind_RightParen && context->current_token.kind != TokenKind_Invalid) {
		const NodeResult new_expr_node = parse_expression(input, context);
		current_child->right_sibling_index = new_expr_node.node_id;
		result.node->child_count += 1;

		current_child = new_expr_node.node;
	}

	if (context->current_token.kind != TokenKind_RightParen) {
		return (NodeResult){0};
	}
	Token right_paren_token = context->current_token;
	parser_eat_token(input, context); // eat right paren

	result.node->span = span_extend(left_paren_token.span, right_paren_token.span);
	result.ok = true;
	return result;
}

// sexpr | atom
static NodeResult parse_expression(ParserInput input, ParserContext *context)
{
	if (context->current_token.kind == TokenKind_LeftParen) {
		return parse_s_expression(input, context);
	} else {
		return parse_atom(input, context);
	}
}

// sexpr*
ParserResult parse_module(Arena *memory, const Token *tokens)
{
	ParserResult result = (ParserResult){0};

	uint32_t token_count = array_count(tokens);
	ASSERT(token_count > 0);

	ParserInput input = (ParserInput){0};
	input.nodes = array_init(memory);
	input.tokens = tokens;

	ParserContext context = (ParserContext){0};
	context.i_current_token = 0;
	context.current_token = input.tokens[0];

	NodeResult root = parser_push_ast_node_sexpr(input, &context, INVALID_NODE_INDEX);
	if (root.ok == false) {
		return result;
	}

	AstNode *current_child = NULL;
	while (context.i_current_token < token_count) {
		NodeResult expr = parse_s_expression(input, &context);
		if (root.ok == false) {
			return result;
		}

		root.node->child_count += 1;
		if (current_child == NULL) {
			root.node->left_child_index = expr.node_id;
		} else {
			current_child->right_sibling_index = expr.node_id;
		}
		current_child = expr.node;
	}

	result.nodes = input.nodes;
	return result;
}

// Typed nodes parsing
TypeID parse_type(CompilationUnit *compunit, CompilerModule *module, const AstNode *node)
{
	if (ast_is_atom(node)) {
		// The type is just an identifier, find the corresponding builtin type or named type
		const Token identifier = ast_get_token(compunit->tokens, node);
		sv identifier_str = string_pool_get(&compunit->string_pool, identifier.data.sid);;

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

		const AstNode *child0 = ast_get_left_child(compunit->nodes, node);
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
		const AstNode *child1 = ast_get_right_sibling(compunit->nodes, child0);
		// Assert that there is only one argument
		if (ast_has_right_sibling(child1)) {
			const AstNode *child2 = ast_get_right_sibling(compunit->nodes, child1);
			INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
			compunit->error.span = child2->span;
			return UNIT_TYPE;
		}

		const Token token = ast_get_token(compunit->tokens, child0);
		const sv token_str = string_pool_get(&compunit->string_pool, token.data.sid);
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
		compunit->error.span = token.span;
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

	const AstNode *define_token_node = ast_get_left_child(compunit->nodes, node);
	const AstNode *name_type_node = ast_get_right_sibling(compunit->nodes, define_token_node);
	const AstNode *arglist_node = ast_get_right_sibling(compunit->nodes, name_type_node);

	// Get the function name node
	if (name_type_node->child_count != 2) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
		compunit->error.span = node->span;
		return;
	}

	const AstNode *function_name_node = ast_get_left_child(compunit->nodes, name_type_node);
	if (!ast_is_atom(function_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
		compunit->error.span = node->span;
		return;
	}

	output->function_name_token = ast_get_token(compunit->tokens, function_name_node);
	output->return_type_node = ast_get_right_sibling(compunit->nodes, function_name_node);
	output->arglist_node = arglist_node;

	// Arguments are laid out as (name1 type1 name2 type2 ...)
	uint32_t i_arg_node = arglist_node->left_child_index;
	uint32_t args_length = 0;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_name_node = ast_get_node(compunit->nodes, i_arg_node);
		const Token arg_name = ast_get_token(compunit->tokens, arg_name_node);
		const bool arg_is_an_identifier = ast_is_atom(arg_name_node) && arg_name.kind == TokenKind_Identifier;
		if (!arg_is_an_identifier) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			compunit->error.span = arglist_node->span;
			return;
		}

		const AstNode *arg_type_node = ast_get_right_sibling(compunit->nodes, arg_name_node);
		if (!ast_has_right_sibling(arg_name_node)) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
			compunit->error.span = arglist_node->span;
			return;
		}

		output->arg_identifiers[args_length] = arg_name;
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
	output->body_node = ast_get_right_sibling(compunit->nodes, output->arglist_node);
}

// (struct <name> (<field name> <field type>)+)
void parse_struct(CompilationUnit *compunit, const AstNode *node, StructNode *output)
{
	const AstNode *struct_token_node = ast_get_left_child(compunit->nodes, node);

	// Get struct name node
	const AstNode *struct_name_node = ast_get_right_sibling(compunit->nodes, struct_token_node);
	if (!ast_has_right_sibling(struct_token_node) || !ast_is_atom(struct_name_node)) {
		INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
		compunit->error.span = node->span;
		return;
	}
	output->struct_name_token = ast_get_token(compunit->tokens, struct_name_node);

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
		const AstNode *field_node = ast_get_node(compunit->nodes, i_field_node);
		// Get the field name
		const AstNode *field_identifier_node = ast_get_left_child(compunit->nodes, field_node);
		if (!ast_has_left_child(field_node)) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			compunit->error.span = field_node->span;
			return;
		}
		const Token field_identifier_token = ast_get_token(compunit->tokens, field_identifier_node);
		const bool is_an_identifier_token_node =
			ast_is_atom(field_identifier_node) && field_identifier_token.kind == TokenKind_Identifier;
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
		const AstNode *field_type_node = ast_get_right_sibling(compunit->nodes, field_identifier_node);
		if (fields_length > MAX_STRUCT_FIELDS) {
			INIT_ERROR(&compunit->error, ErrorCode_Fatal);
			compunit->error.span = field_identifier_node->span;
			return;
		}

		output->field_identifiers[fields_length] = field_identifier_token;
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

	const AstNode *if_token_node = ast_get_left_child(compunit->nodes, node);
	output->cond_expr_node = ast_get_right_sibling(compunit->nodes, if_token_node);
	output->then_expr_node = ast_get_right_sibling(compunit->nodes, output->cond_expr_node);
	output->else_expr_node = ast_get_right_sibling(compunit->nodes, output->then_expr_node);
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

	const AstNode *let_token_node = ast_get_left_child(compunit->nodes, node);
	const AstNode *name_node = ast_get_right_sibling(compunit->nodes, let_token_node);
	output->name_token = ast_get_token(compunit->tokens, name_node);
	output->value_node = ast_get_right_sibling(compunit->nodes, name_node);
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

	const AstNode *field_token_node = ast_get_left_child(compunit->nodes, node);
	const AstNode *expr_node = ast_get_right_sibling(compunit->nodes, field_token_node);
	const AstNode *field_identifier_node = ast_get_right_sibling(compunit->nodes, expr_node);

	const Token field_identifier_token = ast_get_token(compunit->tokens, field_identifier_node);
	if (!ast_is_atom(field_identifier_node) || field_identifier_token.kind != TokenKind_Identifier) {
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

	const AstNode *ptr_offset_token_node = ast_get_left_child(compunit->nodes, node);
	const AstNode *return_type_node = ast_get_right_sibling(compunit->nodes, ptr_offset_token_node);
	const AstNode *base_pointer_node = ast_get_right_sibling(compunit->nodes, return_type_node);
	const AstNode *offset_node = ast_get_right_sibling(compunit->nodes, base_pointer_node);

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
	
	const AstNode *current_node = ast_get_left_child(compunit->nodes, node);
	for (uint32_t i = 0; i < n; ++i) {
		current_node = ast_get_right_sibling(compunit->nodes, current_node);
		out_nodes[i] = current_node;
	}
}

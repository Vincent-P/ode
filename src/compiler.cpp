#include "compiler.h"
#include "lexer.h"
#include "parser.h"
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
	vec<uint32_t> line_endings;
};

void text_input_get_line_col(const TextInput *input, uint32_t at, uint32_t *line, uint32_t *col)
{
	uint32_t i_line = 0;
	uint32_t last_line_ending = 0;
	for (; i_line + 1 < input->line_endings.length; ++i_line) {
		const uint32_t *it = vec_at(&input->line_endings, i_line);
		if (*it > at) {
			break;
		}
		last_line_ending = *it;
	}

	*line = i_line;
	*col = at - last_line_ending;
}

TypeID type_id_pointer_from(TypeID inner_type)
{
	if (type_id_is_pointer(inner_type)) {
		inner_type.pointer.indirection_count += 1;
		return inner_type;
	} else if (type_id_is_builtin(inner_type)) {
		TypeID pointer_type = {};
		pointer_type.pointer = {};
		pointer_type.pointer.builtin_kind = BuiltinTypeKind::Pointer;
		pointer_type.pointer.pointee_builtin_kind = inner_type.builtin.kind;
		pointer_type.pointer.indirection_count = 1;
		return pointer_type;
	} else if (type_id_is_user_defined(inner_type)) {
		TypeID pointer_type = {};
		pointer_type.pointer = {};
		pointer_type.pointer.builtin_kind = BuiltinTypeKind::Pointer;
		pointer_type.pointer.indirection_count = 1;
		pointer_type.pointer.user_defined_index = inner_type.user_defined.index;
		return pointer_type;
	}
	
	return UNIT_TYPE;
}

TypeID type_id_deref_pointer(TypeID pointer_type)
{
	if (pointer_type.pointer.indirection_count > 1) {
		TypeID pointed_type = pointer_type;
		pointed_type.pointer.indirection_count += 1;
		return pointed_type;
	} else if (pointer_type.pointer.pointee_builtin_kind != BuiltinTypeKind::Unit) {
		return type_id_new_builtin(pointer_type.pointer.pointee_builtin_kind);
	} else {
		return type_id_new_user_defined(pointer_type.pointer.user_defined_index);
	}
}

struct Compiler
{
	vec<Module> modules;
};

struct LexicalScope
{
	sv *variables_name;
	TypeID *variables_type;
	uint32_t variables_length;
};

struct CompilerState
{
	// input
	TextInput input;
	vec<AstNode> nodes; // TODO: slice type? we don't own these
	vec<Token> tokens;
	// compiler data
	Module *current_module;
	vec<LexicalScope> scopes;
	uint32_t memory_stack_depth;
	// error handling
	Result result;
	const char *result_file;
	int result_file_line;
	span error;
	Token got_token;
	TypeID expected_type;
	TypeID got_type;
};

// parsing functions

static TypeID parse_type(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	if (ast_is_atom(node)) {
		// The type is just an identifier, find the corresponding builtin type or named type
		const Token *identifier = ast_get_token(&compstate->tokens, node);
		sv identifier_str = sv_substr(compstate->input.text, identifier->span);

		// Search builtin types
		for (uint32_t i_builtin_type = 0; i_builtin_type < ARRAY_LENGTH(BuiltinTypeKind_str); ++i_builtin_type) {
			const char *builtin_str = BuiltinTypeKind_str[i_builtin_type];
			if (sv_equals(identifier_str, sv_from_null_terminated(builtin_str))) {
				return type_id_new_builtin(BuiltinTypeKind(i_builtin_type));
			}
		}

		// Search named types
		for (uint32_t i_type = 0; i_type < compstate->current_module->types_length; ++i_type) {
			if (sv_equals(compstate->current_module->types[i_type].name, identifier_str)) {
				return type_id_new_user_defined(i_type);
			}
		}

		// We haven't found any named type
		compstate->result = Result::CompilerUnknownSymbol;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	} else {
		// The compiler is a S-expression
		if (!ast_has_left_child(node)) {
			// () is a unit type
			return UNIT_TYPE;
		}

		const AstNode *child0 = ast_get_left_child(&compstate->nodes, node);
		// The type is a S-expr, either it is a unit type () or a form starting with a TOKEN
		if (!ast_is_atom(child0)) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = child0->span;
			return UNIT_TYPE;
		}
		// Assert that there is at least one argument
		if (!ast_has_right_sibling(child0)) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			return UNIT_TYPE;
		}
		const AstNode *child1 = ast_get_right_sibling(&compstate->nodes, child0);
		// Assert that there is only one argument
		if (ast_has_right_sibling(child1)) {
			const AstNode *child2 = ast_get_right_sibling(&compstate->nodes, child1);
			compstate->result = Result::CompilerUnexpectedExpression;
			SET_RESULT(compstate->result);
			compstate->error = child2->span;
			return UNIT_TYPE;
		}

		const Token *token = ast_get_token(&compstate->tokens, child0);
		const sv token_str = sv_substr(compstate->input.text, token->span);

		// We only support type S-expr of the forms:
		// (* <type>)
		if (sv_equals(token_str, sv_from_null_terminated("*"))) {
			TypeID inner_type = parse_type(compiler, compstate, child1);
			return type_id_pointer_from(inner_type);
		}

		// We haven't found any builtin
		compstate->result = Result::CompilerUnknownSymbol;
		SET_RESULT(compstate->result);
		compstate->error = token->span;
		return UNIT_TYPE;
	}
}

struct DefineNode
{
	const Token *function_name_token;
	Token arg_identifiers[MAX_ARGUMENTS];
	const AstNode *arg_nodes[MAX_ARGUMENTS];
	const AstNode *return_type_node;
	const AstNode *body_node;
	uint32_t args_length;
	// internal
	const AstNode *arglist_node;
};

// Parse a define function signature without a body
// (define (<name> <return_type>) (<args>*) <expression>+)
void parse_define_sig(Compiler *, CompilerState *compstate, const AstNode *node, DefineNode *output)
{
	if (node->child_count < 3) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	const AstNode *define_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *name_type_node = ast_get_right_sibling(&compstate->nodes, define_token_node);
	const AstNode *arglist_node = ast_get_right_sibling(&compstate->nodes, name_type_node);

	// Get the function name node
	if (name_type_node->child_count != 2) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	const AstNode *function_name_node = ast_get_left_child(&compstate->nodes, name_type_node);
	if (!ast_is_atom(function_name_node)) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	output->function_name_token = ast_get_token(&compstate->tokens, function_name_node);
	output->return_type_node = ast_get_right_sibling(&compstate->nodes, function_name_node);
	output->arglist_node = arglist_node;

	// Arguments are laid out as (name1 type1 name2 type2 ...)
	uint32_t i_arg_node = arglist_node->left_child_index;
	uint32_t args_length = 0;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_name_node = ast_get_node(&compstate->nodes, i_arg_node);
		const Token *arg_name = ast_get_token(&compstate->tokens, arg_name_node);
		const bool arg_is_an_identifier = ast_is_atom(arg_name_node) && arg_name->kind == TokenKind::Identifier;
		if (!arg_is_an_identifier) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return;
		}

		const AstNode *arg_type_node = ast_get_right_sibling(&compstate->nodes, arg_name_node);
		if (!ast_has_right_sibling(arg_name_node)) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return;
		}

		output->arg_identifiers[args_length] = *arg_name;
		output->arg_nodes[args_length] = arg_type_node;
		args_length += 1;
		i_arg_node = arg_type_node->right_sibling_index;
	}
	output->args_length = args_length;
}

void parse_define_body(Compiler *, CompilerState *compstate, const AstNode *node, DefineNode *output)
{
	if (!ast_has_right_sibling(output->arglist_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}
	output->body_node = ast_get_right_sibling(&compstate->nodes, output->arglist_node);
}

struct StructNode
{
	const Token *struct_name_token;
	Token field_identifiers[MAX_STRUCT_FIELDS];
	const AstNode *field_type_nodes[MAX_STRUCT_FIELDS];
	uint32_t fields_length;
};

// (struct <name> (<field name> <field type>)+)
void parse_struct(Compiler *, CompilerState *compstate, const AstNode *node, StructNode *output)
{
	const AstNode *struct_token_node = ast_get_left_child(&compstate->nodes, node);

	// Get struct name node
	const AstNode *struct_name_node = ast_get_right_sibling(&compstate->nodes, struct_token_node);
	if (!ast_has_right_sibling(struct_token_node) || !ast_is_atom(struct_name_node)) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}
	output->struct_name_token = ast_get_token(&compstate->tokens, struct_name_node);

	// Get fields
	// Get the first field (a struct MUST have at least one field)
	if (!ast_has_right_sibling(struct_name_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}
	uint32_t fields_length = 0;
	uint32_t i_field_node = struct_name_node->right_sibling_index;
	while (ast_is_valid(i_field_node)) {
		const AstNode *field_node = ast_get_node(&compstate->nodes, i_field_node);
		// Get the field name
		const AstNode *field_identifier_node = ast_get_left_child(&compstate->nodes, field_node);
		if (!ast_has_left_child(field_node)) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = field_node->span;
			return;
		}
		const Token *field_identifier_token = ast_get_token(&compstate->tokens, field_identifier_node);
		const bool is_an_identifier_token_node =
			ast_is_atom(field_identifier_node) && field_identifier_token->kind == TokenKind::Identifier;
		if (!is_an_identifier_token_node) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = field_identifier_node->span;
			return;
		}
		// Get the field type
		if (!ast_has_right_sibling(field_identifier_node)) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = field_node->span;
			return;
		}
		const AstNode *field_type_node = ast_get_right_sibling(&compstate->nodes, field_identifier_node);
		if (fields_length > MAX_STRUCT_FIELDS) {
			compstate->result = Result::Fatal;
			SET_RESULT(compstate->result);
			compstate->error = field_identifier_node->span;
			return;
		}

		output->field_identifiers[fields_length] = *field_identifier_token;
		output->field_type_nodes[fields_length] = field_type_node;
		fields_length += 1;

		i_field_node = field_node->right_sibling_index;
	}
	output->fields_length = fields_length;
}

struct IfNode
{
	const AstNode *cond_expr_node;
	const AstNode *then_expr_node;
	const AstNode *else_expr_node;
};

// (if <cond_expression> <then_expression> <else_expression>)
void parse_if(Compiler *, CompilerState *compstate, const AstNode *node, IfNode *output)
{
	if (node->child_count != 4) {
		if (node->child_count > 4) {
			compstate->result = Result::CompilerUnexpectedExpression;
		} else {
			compstate->result = Result::CompilerExpectedExpr;
		}
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	const AstNode *if_token_node = ast_get_left_child(&compstate->nodes, node);
	output->cond_expr_node = ast_get_right_sibling(&compstate->nodes, if_token_node);
	output->then_expr_node = ast_get_right_sibling(&compstate->nodes, output->cond_expr_node);
	output->else_expr_node = ast_get_right_sibling(&compstate->nodes, output->then_expr_node);
}

struct LetNode
{
	const Token *name_token;
	const AstNode *value_node;
};

// (let <name> <value expr>)
void parse_let(Compiler *, CompilerState *compstate, const AstNode *node, LetNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			compstate->result = Result::CompilerUnexpectedExpression;
		} else {
			compstate->result = Result::CompilerExpectedExpr;
		}
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	const AstNode *let_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *name_node = ast_get_right_sibling(&compstate->nodes, let_token_node);
	output->name_token = ast_get_token(&compstate->tokens, name_node);
	output->value_node = ast_get_right_sibling(&compstate->nodes, name_node);
}

struct BinaryOpNode
{
	const AstNode *lhs_node;
	const AstNode *rhs_node;
};

void parse_binary_op(Compiler *, CompilerState *compstate, const AstNode *node, BinaryOpNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			compstate->result = Result::CompilerTooManyArgs;
		} else {
			compstate->result = Result::CompilerExpectedExpr;
		}
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}
	const AstNode *op_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *lhs_node = ast_get_right_sibling(&compstate->nodes, op_node);
	const AstNode *rhs_node = ast_get_right_sibling(&compstate->nodes, lhs_node);
	output->lhs_node = lhs_node;
	output->rhs_node = rhs_node;
}

struct UnaryOpNode
{
	const AstNode *value_node;
};

void parse_unary_op(Compiler *, CompilerState *compstate, const AstNode *node, UnaryOpNode *output)
{
	if (node->child_count != 2) {
		if (node->child_count > 2) {
			compstate->result = Result::CompilerTooManyArgs;
		} else {
			compstate->result = Result::CompilerExpectedExpr;
		}
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	const AstNode *op_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *value_node = ast_get_right_sibling(&compstate->nodes, op_node);
	output->value_node = value_node;
}

struct FieldNode
{
	const AstNode *expr_node;
	const Token *field_token;
};

// (field <expr> <member identifier>)
void parse_field(Compiler *, CompilerState *compstate, const AstNode *node, FieldNode *output)
{
	if (node->child_count != 3) {
		if (node->child_count > 3) {
			compstate->result = Result::CompilerTooManyArgs;
		} else {
			compstate->result = Result::CompilerExpectedExpr;
		}
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	const AstNode *field_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *expr_node = ast_get_right_sibling(&compstate->nodes, field_token_node);
	const AstNode *field_identifier_node = ast_get_right_sibling(&compstate->nodes, expr_node);

	const Token *field_identifier_token = ast_get_token(&compstate->tokens, field_identifier_node);
	if (!ast_is_atom(field_identifier_node) || field_identifier_token->kind != TokenKind::Identifier) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = field_identifier_node->span;
		return;
	}

	output->expr_node = expr_node;
	output->field_token = field_identifier_token;
}

struct PtrOffsetNodes
{
	TypeID return_type;
	const AstNode *base_pointer_node;
	const AstNode *offset_node;
};

// (_ptr_offset <type> <base pointer> <offset>)
void parse_ptr_offset(Compiler *compiler, CompilerState *compstate, const AstNode *node, PtrOffsetNodes *output)
{
	if (node->child_count != 4) {
		if (node->child_count > 4) {
			compstate->result = Result::CompilerTooManyArgs;
		} else {
			compstate->result = Result::CompilerExpectedExpr;
		}
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return;
	}

	const AstNode *ptr_offset_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *return_type_node = ast_get_right_sibling(&compstate->nodes, ptr_offset_token_node);
	const AstNode *base_pointer_node = ast_get_right_sibling(&compstate->nodes, return_type_node);
	const AstNode *offset_node = ast_get_right_sibling(&compstate->nodes, base_pointer_node);

	TypeID return_type = parse_type(compiler, compstate, return_type_node);
	if (!type_id_is_pointer(return_type)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->got_type = return_type;
		compstate->error = return_type_node->span;
		return;
	}

	output->return_type = return_type;
	output->base_pointer_node = base_pointer_node;
	output->offset_node = offset_node;
}

// bytecode functions

static void compiler_push_opcode(Compiler *, CompilerState *compstate, OpCodeKind opcode_kind)
{
	Module *current_module = compstate->current_module;
	if (current_module->bytecode_length + 1 >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	current_module->bytecode[current_module->bytecode_length] = uint8_t(opcode_kind);
	current_module->bytecode_length += 1;
}

template <typename T>
static T *compiler_push_scalar(Compiler *, CompilerState *compstate, T value)
{
	Module *current_module = compstate->current_module;
	uint32_t to_write = sizeof(T);
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return nullptr;
	}

	T *bytecode = reinterpret_cast<T *>(current_module->bytecode + current_module->bytecode_length);
	bytecode[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode;
}

static TypeID *compiler_push_type_id(Compiler *comp, CompilerState *compstate, TypeID id)
{
	return compiler_push_scalar(comp, compstate, id);
}

static void compiler_push_sv(Compiler *, CompilerState *compstate, sv value)
{
	Module *current_module = compstate->current_module;

	uint32_t to_write = uint32_t(sizeof(uint32_t) + value.length * sizeof(char));
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(current_module->bytecode + current_module->bytecode_length);
	bytecode_u32[0] = uint32_t(value.length);

	uint8_t *bytecode_u8 =
		reinterpret_cast<uint8_t *>(current_module->bytecode + current_module->bytecode_length + sizeof(uint32_t));
	for (uint32_t i = 0; i < value.length; ++i)
		bytecode_u8[i] = uint8_t(value.chars[i]);
	current_module->bytecode_length += to_write;
}

static void compiler_bytecode_load_constant_u32(Compiler *compiler, CompilerState *compstate, uint8_t i_constant)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::LoadConstantU32);
	compiler_push_scalar(compiler, compstate, i_constant);
}

static void compiler_bytecode_load_constant_str(Compiler *compiler, CompilerState *compstate, uint8_t i_constant)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::LoadConstantStr);
	compiler_push_scalar(compiler, compstate, i_constant);
}

static void compiler_bytecode_call(Compiler *compiler, CompilerState *compstate, uint8_t i_function)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::Call);
	compiler_push_scalar(compiler, compstate, i_function);
}

static void compiler_bytecode_call_foreign(Compiler *compiler, CompilerState *compstate)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::CallForeign);
}

static void compiler_bytecode_ret(Compiler *compiler, CompilerState *compstate)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::Ret);
}

static uint32_t *compiler_bytecode_conditional_jump(Compiler *compiler, CompilerState *compstate, uint32_t addr)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::ConditionalJump);
	return compiler_push_scalar(compiler, compstate, addr);
}

static uint32_t *compiler_bytecode_jump(Compiler *compiler, CompilerState *compstate, uint32_t addr)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::Jump);
	return compiler_push_scalar(compiler, compstate, addr);
}

static void compiler_bytecode_store_local(Compiler *compiler, CompilerState *compstate, uint8_t i_local)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::StoreLocal);
	compiler_push_scalar(compiler, compstate, i_local);
}

static void compiler_bytecode_load_local(Compiler *compiler, CompilerState *compstate, uint8_t i_local)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::LoadLocal);
	compiler_push_scalar(compiler, compstate, i_local);
}

static void compiler_bytecode_stack_alloc(Compiler *compiler, CompilerState *compstate, TypeID pointed_memory_type)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::StackAlloc);
	compiler_push_type_id(compiler, compstate, pointed_memory_type);
}

static void compiler_bytecode_load(Compiler *compiler, CompilerState *compstate, TypeID expr_type_id)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::Load);
	compiler_push_type_id(compiler, compstate, expr_type_id);
}

static void compiler_bytecode_store(Compiler *compiler, CompilerState *compstate, TypeID expr_type_id)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::Store);
	compiler_push_type_id(compiler, compstate, expr_type_id);
}

static void compiler_bytecode_ptr_offset(Compiler *compiler, CompilerState *compstate)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::PtrOffset);
}

static void compiler_bytecode_debug_label(Compiler *compiler, CompilerState *compstate, sv label)
{
	compiler_push_opcode(compiler, compstate, OpCodeKind::DebugLabel);
	compiler_push_sv(compiler, compstate, label);
}

template <typename Lambda>
static Function *compiler_compile_function(Compiler *compiler,
	CompilerState *compstate,
	sv function_name,
	TypeID return_type,
	Token *arg_identifiers,
	TypeID *arg_types,
	uint32_t args_length,
	Lambda compile_body_fn)
{
	Module *current_module = compstate->current_module;
	for (uint32_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, function_name)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			SET_RESULT(compstate->result);
			// Actually it should be possible to recompile a function if signature has not changed.
			return nullptr;
		}
	}

	if (current_module->functions_length + 1 > current_module->functions_capacity) {
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
	compiler_bytecode_debug_label(compiler, compstate, function_name);
	// Create a variable scope
	compiler_push_scope(compiler, compstate);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		compiler_bytecode_store_local(compiler, compstate, uint8_t(args_length - 1 - i_arg));
	}

	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		TypeID arg_type = arg_types[i_arg];
		function->arg_types[function->arg_count] = arg_types[i_arg];
		function->arg_count += 1;

		uint32_t i_variable = 0;
		if (!compiler_push_variable(compstate, &arg_identifiers[i_arg], arg_type, &i_variable)) {
			return nullptr;
		}
	}

	// <-- Compile the body
	TypeID body_type = compile_body_fn();

	compiler_pop_scope(compiler, compstate);
	compiler_bytecode_ret(compiler, compstate);
	if (compstate->result != Result::Ok) {
		return nullptr;
	}

	bool valid_return_type = type_similar(compstate, return_type, body_type);
	if (!valid_return_type) {
		printf("%.*s\n", int(function->name.length), function->name.chars);
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = return_type;
		compstate->got_type = body_type;
	}

	return function;
}

bool type_similar(CompilerState *, TypeID lhs_id, TypeID rhs_id)
{
	return lhs_id.raw == rhs_id.raw;
}

uint32_t type_get_size(CompilerState *state, TypeID id)
{
	if (id.builtin.is_user_defined != 0) {
		if (id.user_defined.index >= state->current_module->types_length) {
			state->result = Result::Fatal;
			SET_RESULT(state->result);
			return 0;
		}
		return state->current_module->types[id.user_defined.index].size;
	} else {
		return BuiltinTypeKind_size[uint32_t(id.builtin.kind)];
	}
}

static constexpr uint32_t SCOPE_MAX_VARIABLES = 16;

void compiler_push_scope(Compiler *, CompilerState *compstate)
{
	if (compstate->scopes.length >= compstate->scopes.capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	LexicalScope new_scope = {};
	new_scope.variables_name = (sv *)calloc(SCOPE_MAX_VARIABLES, sizeof(sv));
	new_scope.variables_type = (TypeID *)calloc(SCOPE_MAX_VARIABLES, sizeof(TypeID));
	vec_append(&compstate->scopes, new_scope);
}

void compiler_pop_scope(Compiler *, CompilerState *compstate)
{
	if (compstate->scopes.length == 0) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	uint32_t last_scope_index = compstate->scopes.length - 1;
	LexicalScope *current_scope = vec_at(&compstate->scopes, last_scope_index);
	free(current_scope->variables_name);
	free(current_scope->variables_type);
	vec_swap_remove(&compstate->scopes, last_scope_index);
}

bool compiler_push_variable(
	CompilerState *compstate, const Token *identifier_token, TypeID type, uint32_t *i_variable_out)
{
	if (compstate->scopes.length >= compstate->scopes.capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return false;
	}

	uint32_t last_scope_index = compstate->scopes.length - 1;
	LexicalScope *current_scope = vec_at(&compstate->scopes, last_scope_index);
	if (current_scope->variables_length >= SCOPE_MAX_VARIABLES) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return false;
	}

	uint32_t i_new_variable = current_scope->variables_length;
	current_scope->variables_length += 1;

	sv name_str = sv_substr(compstate->input.text, identifier_token->span);

	current_scope->variables_name[i_new_variable] = name_str;
	current_scope->variables_type[i_new_variable] = type;

	*i_variable_out = i_new_variable;
	return true;
}

bool compiler_lookup_variable(
	CompilerState *compstate, const Token *identifier_token, TypeID *type_out, uint32_t *i_variable_out)
{
	if (compstate->scopes.length == 0) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return false;
	}

	sv tofind_name = sv_substr(compstate->input.text, identifier_token->span);

	for (uint32_t i_scope = compstate->scopes.length - 1; i_scope < compstate->scopes.length; --i_scope) {
		LexicalScope *scope = vec_at(&compstate->scopes, i_scope);

		uint32_t i_found = ~uint32_t(0);
		TypeID found_type = UNIT_TYPE;
		for (uint32_t i_variable = 0; i_variable < scope->variables_length; ++i_variable) {
			sv variable_name = scope->variables_name[i_variable];
			if (sv_equals(tofind_name, variable_name)) {
				i_found = i_variable;
				found_type = scope->variables_type[i_variable];
			}
		}

		if (i_found < scope->variables_length) {
			*type_out = found_type;
			*i_variable_out = i_found;
			return true;
		}
	}

	return false;
}

// compiler
TypeID compile_sexpr(Compiler *compiler, CompilerState *compstate, const AstNode *node);

//

static uint32_t compile_find_or_add_constant(Compiler *, CompilerState *compstate, uint32_t value)
{
	// Find or add the number literal to the constant pool
	// TODO: It's slow.
	uint32_t i_constant = 0;
	uint32_t constants_u32_length = compstate->current_module->constants_u32_length;
	for (; i_constant < constants_u32_length; ++i_constant) {
		uint32_t constant_u32 = compstate->current_module->constants_u32[i_constant];
		if (value == constant_u32) {
			break;
		}
	}

	// The constant string was not found, add it.
	if (i_constant == constants_u32_length) {
		if (constants_u32_length >= compstate->current_module->constants_u32_capacity) {

			compstate->result = Result::Fatal;
			SET_RESULT(compstate->result);
			return INVALID_NODE_INDEX;
		}
		compstate->current_module->constants_u32_length += 1;
		compstate->current_module->constants_u32[i_constant] = value;
	}
	return i_constant;
}

static TypeID compile_atom(Compiler *compiler, CompilerState *compstate, const Token *token)
{
	sv token_sv = sv_substr(compstate->input.text, token->span);

	if (token->kind == TokenKind::Identifier) {
		// Refer a declared variable
		// <identifier>
		TypeID ty = UNIT_TYPE;
		uint32_t i_variable = 0;
		if (!compiler_lookup_variable(compstate, token, &ty, &i_variable)) {
			compstate->result = Result::CompilerUnknownSymbol;
			SET_RESULT(compstate->result);
			compstate->error = token->span;
			return UNIT_TYPE;
		}
		compiler_bytecode_load_local(compiler, compstate, uint8_t(i_variable));
		return ty;
	} else if (token->kind == TokenKind::Number) {
		// An integer constant
		// <number>
		int32_t token_number = sv_to_int(token_sv);
		uint32_t i_constant = compile_find_or_add_constant(compiler, compstate, uint32_t(token_number));
		compiler_bytecode_load_constant_u32(compiler, compstate, uint8_t(i_constant));
		return type_id_new_builtin(BuiltinTypeKind::Int);
	} else if (token->kind == TokenKind::StringLiteral) {
		// A string literal
		// "str"
		sv value_sv = sv_substr(token_sv, span{1, uint32_t(token_sv.length) - 2});

		// Find or add the string literal to the constant pool
		// TODO: It's slow.
		uint32_t i_constant = 0;
		uint32_t constant_strings_length = compstate->current_module->constant_strings_length;
		for (; i_constant < constant_strings_length; ++i_constant) {
			sv constant_sv = compstate->current_module->constant_strings[i_constant];
			if (sv_equals(value_sv, constant_sv)) {
				break;
			}
		}
		// The constant string was not found, add it.
		if (i_constant == constant_strings_length) {
			if (constant_strings_length >= compstate->current_module->constant_strings_capacity) {

				compstate->result = Result::Fatal;
				SET_RESULT(compstate->result);
				return UNIT_TYPE;
			}
			compstate->current_module->constant_strings_length += 1;
			compstate->current_module->constant_strings[i_constant] = value_sv;
		}

		compiler_bytecode_load_constant_str(compiler, compstate, uint8_t(i_constant));

		TypeID new_type_id = type_id_new_builtin(BuiltinTypeKind::Str);
		return new_type_id;
	} else {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return UNIT_TYPE;
	}
}

// <identifier> | <number> | <s-expression>
TypeID compile_expr(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	if (ast_is_atom(node)) {
		const Token *token = ast_get_token(&compstate->tokens, node);
		TypeID return_type = compile_atom(compiler, compstate, token);
		if (type_id_is_user_defined(return_type)) {
			compstate->memory_stack_depth += 1;
		}
		return return_type;
	} else if (ast_has_left_child(node)) {
		TypeID return_type = compile_sexpr(compiler, compstate, node);
		if (type_id_is_user_defined(return_type)) {
			compstate->memory_stack_depth += 1;
		}
		return return_type;

	} else {
		// () unit value
		return UNIT_TYPE;
	}
}

TypeID compile_sexprs_return_last(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *first_expr_node = node;
	TypeID return_type = compile_expr(compiler, compstate, first_expr_node);

	uint32_t i_next_expr_node = first_expr_node->right_sibling_index;
	while (ast_is_valid(i_next_expr_node)) {
		const AstNode *next_expr_node = ast_get_node(&compstate->nodes, i_next_expr_node);
		return_type = compile_expr(compiler, compstate, next_expr_node);
		i_next_expr_node = next_expr_node->right_sibling_index;
	}

	return return_type;
}

// Defines a new function
// (define (<name> <return_type>) (<args>*) <expression>+)
TypeID compile_define(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	DefineNode define_node = {};
	parse_define_sig(compiler, compstate, node, &define_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}
	parse_define_body(compiler, compstate, node, &define_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	sv function_name_token_str = sv_substr(compstate->input.text, define_node.function_name_token->span);

	// -- Type checking
	TypeID return_type = parse_type(compiler, compstate, define_node.return_type_node);
	TypeID arg_types[MAX_ARGUMENTS] = {};
	for (uint32_t i_arg = 0; i_arg < define_node.args_length; ++i_arg) {
		arg_types[i_arg] = parse_type(compiler, compstate, define_node.arg_nodes[i_arg]);
	}

	Function *new_function = compiler_compile_function(compiler,
		compstate,
		function_name_token_str,
		return_type,
		define_node.arg_identifiers,
		arg_types,
		define_node.args_length,
		[&]() -> TypeID {
			// <-- Compile the body
			TypeID body_type = compile_sexprs_return_last(compiler, compstate, define_node.body_node);
			return body_type;
		});

	if (new_function == nullptr) {
		return UNIT_TYPE;
	}
	return new_function->return_type;
}

// Defines a new foreign function
// (define-foreign (<name> <return_type>) (<args>))
TypeID compile_define_foreign(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	DefineNode nodes = {};
	parse_define_sig(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}
	sv function_name_token_str = sv_substr(compstate->input.text, nodes.function_name_token->span);

	// -- Type checking
	Module *current_module = compstate->current_module;
	for (uint32_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, function_name_token_str)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			// Actually it should be possible to recompile a function if signature has not changed.
			return UNIT_TYPE;
		}
	}
	if (current_module->functions_length + 1 >= current_module->functions_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}

	// Create a new function symbol
	Function *function = current_module->functions + current_module->functions_length;
	function->name = function_name_token_str;
	function->address = current_module->bytecode_length;
	function->is_foreign = true;

	current_module->functions_length += 1;

	// -- Compiling
	TypeID return_type = parse_type(compiler, compstate, nodes.return_type_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}
	function->return_type = return_type;

	// Add a debug label to identify functions easily in the bytecode
	compiler_bytecode_debug_label(compiler, compstate, function_name_token_str);
	// Create a variable scope
	compiler_push_scope(compiler, compstate);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint32_t i_arg = 0; i_arg < nodes.args_length; ++i_arg) {
		compiler_bytecode_store_local(compiler, compstate, uint8_t(nodes.args_length - 1 - i_arg));
	}
	// push argument variables
	for (uint32_t i_arg = 0; i_arg < nodes.args_length; ++i_arg) {
		TypeID arg_type = parse_type(compiler, compstate, nodes.arg_nodes[i_arg]);
		function->arg_types[function->arg_count] = arg_type;
		function->arg_count += 1;

		uint32_t i_variable = 0;
		if (!compiler_push_variable(compstate, &nodes.arg_identifiers[i_arg], arg_type, &i_variable)) {
			return UNIT_TYPE;
		}
	}
	compiler_bytecode_call_foreign(compiler, compstate);
	compiler_pop_scope(compiler, compstate);
	compiler_bytecode_ret(compiler, compstate);
	return return_type;
}

// Defines a new struct
TypeID compile_struct(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	StructNode nodes = {};
	parse_struct(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}
	sv struct_name_token_str = sv_substr(compstate->input.text, nodes.struct_name_token->span);

	// -- Type checking
	// Check if the type is already defined
	for (uint32_t i_type = 0; i_type < compstate->current_module->types_length; ++i_type) {
		UserDefinedType *type = compstate->current_module->types + i_type;
		if (sv_equals(type->name, struct_name_token_str)) {
			compstate->result = Result::CompilerDuplicateSymbol;
			SET_RESULT(compstate->result);
			compstate->error = node->span;
			// Actually it should be possible to recompile a function if signature has not changed.
			return UNIT_TYPE;
		}
	}
	// Not enough space to add a type
	if (compstate->current_module->types_length + 1 >= compstate->current_module->types_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}

	// -- Create a new structure type
	TypeID fields_type[MAX_STRUCT_FIELD] = {};

	TypeID struct_type_id = type_id_new_user_defined(compstate->current_module->types_length);
	compstate->current_module->types_length += 1;

	UserDefinedType *struct_type = compstate->current_module->types + struct_type_id.user_defined.index;
	*struct_type = {};
	struct_type->size = 0;
	struct_type->name = struct_name_token_str;

	uint32_t struct_size = 0;
	for (uint32_t i_field = 0; i_field < nodes.fields_length; ++i_field) {
		TypeID field_type = parse_type(compiler, compstate, nodes.field_type_nodes[i_field]);
		if (compstate->result != Result::Ok) {
			return UNIT_TYPE;
		}

		struct_type->field_types[i_field] = field_type;
		struct_type->field_names[i_field] = sv_substr(compstate->input.text, nodes.field_identifiers[i_field].span);
		struct_type->field_offsets[i_field] = struct_size;

		struct_size += type_get_size(compstate, field_type);
		fields_type[i_field] = field_type;
	}

	struct_type->field_count = nodes.fields_length;
	struct_type->size = struct_size;

	return struct_type_id;
}

// Conditional branch
TypeID compile_if(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	IfNode nodes = {};
	parse_if(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// Compile the condition first,
	/*TypeID cond_expr =*/compile_expr(compiler, compstate, nodes.cond_expr_node);
	// TODO: no type checking?
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// If true, jump to the true branch (patch the jump adress later)
	uint32_t *jump_to_true_branch = compiler_bytecode_conditional_jump(compiler, compstate, 0);

	// Then compile the else branch, because the condition was false
	TypeID else_expr = compile_expr(compiler, compstate, nodes.else_expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// Jump over the true branch (patch the jump adress later)
	uint32_t *jump_to_end = compiler_bytecode_jump(compiler, compstate, 0);

	// Compile the true branch
	const uint32_t then_branch_address = compstate->current_module->bytecode_length;
	TypeID then_expr = compile_expr(compiler, compstate, nodes.then_expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	const uint32_t end_address = compstate->current_module->bytecode_length;
	*jump_to_true_branch = uint32_t(then_branch_address);
	*jump_to_end = uint32_t(end_address);

	bool valid_return_type = type_similar(compstate, then_expr, else_expr);
	if (!valid_return_type) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		compstate->expected_type = then_expr;
		compstate->got_type = else_expr;
	}

	return then_expr;
}

TypeID compile_let(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	LetNode nodes = {};
	parse_let(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	TypeID expr_type = compile_expr(compiler, compstate, nodes.value_node);
	uint32_t i_variable = 0;
	if (!compiler_push_variable(compstate, nodes.name_token, expr_type, &i_variable)) {
		return UNIT_TYPE;
	}

	compiler_bytecode_store_local(compiler, compstate, uint8_t(i_variable));
	return UNIT_TYPE;
}

// (begin <expr1> <expr2> ...)
TypeID compile_begin(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *begin_node = ast_get_left_child(&compstate->nodes, node);
	// A begin expression must have at least one expr
	if (!ast_has_right_sibling(begin_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}

	const AstNode *first_sexpr = ast_get_right_sibling(&compstate->nodes, begin_node);
	return compile_sexprs_return_last(compiler, compstate, first_sexpr);
}

// (<op> <lhs> <rhs>)
TypeID compile_binary_opcode(
	Compiler *compiler, CompilerState *compstate, const AstNode *node, TypeID type, OpCodeKind opcode)
{
	// -- Parsing
	BinaryOpNode nodes = {};
	parse_binary_op(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// -- Type checking
	TypeID lhs = compile_expr(compiler, compstate, nodes.lhs_node);
	TypeID rhs = compile_expr(compiler, compstate, nodes.rhs_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	bool lhs_valid = type_similar(compstate, lhs, type);
	bool rhs_valid = type_similar(compstate, rhs, type);

	if (!lhs_valid) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = type;
		compstate->got_type = lhs;
		compstate->error = nodes.lhs_node->span;
		return UNIT_TYPE;
	}

	if (!rhs_valid) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = type;
		compstate->got_type = rhs;
		compstate->error = nodes.rhs_node->span;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, compstate, opcode);

	return lhs;
}

// Add two integers
TypeID compile_iadd(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	TypeID int_type_id = type_id_new_builtin(BuiltinTypeKind::Int);
	return compile_binary_opcode(compiler, compstate, node, int_type_id, OpCodeKind::IAdd);
}

// Substract two integers
TypeID compile_isub(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	TypeID int_type_id = type_id_new_builtin(BuiltinTypeKind::Int);
	return compile_binary_opcode(compiler, compstate, node, int_type_id, OpCodeKind::ISub);
}

// Compare two integers
// (<= <lhs> <rhs>)
TypeID compile_ltethan(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	TypeID int_type_id = type_id_new_builtin(BuiltinTypeKind::Int);
	return compile_binary_opcode(compiler, compstate, node, int_type_id, OpCodeKind::ILessThanEq);
}

// Load a value at the given memory address
// (load <addr>)
TypeID compile_load(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	UnaryOpNode nodes = {};
	parse_unary_op(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	TypeID addr_type_id = compile_expr(compiler, compstate, nodes.value_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// Typecheck	
	if (!type_id_is_pointer(addr_type_id)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = nodes.value_node->span;
		compstate->expected_type = {};
		compstate->got_type = addr_type_id;
		return UNIT_TYPE;
	}

	TypeID pointed_type_id = type_id_deref_pointer(addr_type_id);
	compiler_bytecode_load(compiler, compstate, pointed_type_id);
	return pointed_type_id;
}

// Store a value at the given memory address
// (store <addr> <value>)
TypeID compile_store(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	BinaryOpNode nodes = {};
	parse_binary_op(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	TypeID addr_type_id = compile_expr(compiler, compstate, nodes.lhs_node);
	TypeID expr_type_id = compile_expr(compiler, compstate, nodes.rhs_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}
	
	// Typecheck
	TypeID expected_pointer_type = type_id_pointer_from(expr_type_id);
	if (!type_similar(compstate, expected_pointer_type, addr_type_id)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = nodes.lhs_node->span;
		compstate->expected_type = expected_pointer_type;
		compstate->got_type = addr_type_id;
		return UNIT_TYPE;
	}

	compiler_bytecode_store(compiler, compstate, expr_type_id);
	return UNIT_TYPE;
}

// Returns the size of a type
TypeID compile_sizeof(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	UnaryOpNode nodes = {};
	parse_unary_op(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	TypeID expr_type = parse_type(compiler, compstate, nodes.value_node);
	uint32_t type_size = type_get_size(compstate, expr_type);
	uint32_t i_constant = compile_find_or_add_constant(compiler, compstate, type_size);
	compiler_bytecode_load_constant_u32(compiler, compstate, uint8_t(i_constant));
	return type_id_new_builtin(BuiltinTypeKind::Int);
}

TypeID compile_field_offset(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	BinaryOpNode nodes = {};
	parse_binary_op(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// -- Typecheck
	TypeID expr_type = parse_type(compiler, compstate, nodes.lhs_node);
	if (!type_id_is_user_defined(expr_type)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = nodes.lhs_node->span;
		compstate->got_type = expr_type;
		return UNIT_TYPE;
	}

	bool is_an_identifier = ast_is_atom(nodes.rhs_node);
	const Token *field_token = vec_at(&compstate->tokens, nodes.rhs_node->atom_token_index);
	is_an_identifier = is_an_identifier && field_token->kind == TokenKind::Identifier;
	if (!is_an_identifier) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = nodes.rhs_node->span;
		return UNIT_TYPE;
	}
	sv field_identifier_str = sv_substr(compstate->input.text, field_token->span);

	// -- Find field offset
	if (expr_type.user_defined.index >= compstate->current_module->types_length) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return UNIT_TYPE;
	}
	
	UserDefinedType *type = compstate->current_module->types + expr_type.user_defined.index;
	for (uint32_t i_field = 0; i_field < type->field_count; ++i_field) {
		if (sv_equals(type->field_names[i_field], field_identifier_str)) {
			uint32_t i_constant = compile_find_or_add_constant(compiler, compstate, type->field_offsets[i_field]);
			compiler_bytecode_load_constant_u32(compiler, compstate, uint8_t(i_constant));
			return type_id_new_builtin(BuiltinTypeKind::Int);
		}
	}
	
	compstate->result = Result::CompilerUnknownSymbol;
	SET_RESULT(compstate->result);
	compstate->error = field_token->span;
	return UNIT_TYPE;
}

// Allocate memory on the stack (_stack_alloc <type> <size>)
TypeID compile_stack_alloc(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	BinaryOpNode nodes = {};
	parse_binary_op(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	TypeID type_of_pointed_memory = parse_type(compiler, compstate, nodes.lhs_node);
	/*TypeID size_type =*/ compile_expr(compiler, compstate, nodes.rhs_node);

	// TODO: We need to bound check the alloc, <size> > sizeof(<type>
	
	compiler_bytecode_stack_alloc(compiler, compstate, type_of_pointed_memory);
	return type_id_pointer_from(type_of_pointed_memory);
}

TypeID compile_ptr_offset(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	PtrOffsetNodes nodes = {};
	parse_ptr_offset(compiler, compstate, node, &nodes);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	TypeID return_type = nodes.return_type;
	/*TypeID base_pointer_type =*/ compile_expr(compiler, compstate, nodes.base_pointer_node);
	
	// Check that the provided offset is an int
	TypeID offset_type = compile_expr(compiler, compstate, nodes.offset_node);
	TypeID expected_offset_type = type_id_new_builtin(BuiltinTypeKind::Int);
	if (!type_similar(compstate, offset_type, expected_offset_type)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = nodes.offset_node->span;
		compstate->got_type = offset_type;
		compstate->expected_type = expected_offset_type;
		return UNIT_TYPE;
	}
	
	compiler_bytecode_ptr_offset(compiler, compstate);
	return return_type;
}

using CompstateBuiltin = TypeID (*)(Compiler *, CompilerState *, const AstNode *);

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
	compile_store,
	compile_load,
	compile_sizeof,
	compile_field_offset,
	compile_stack_alloc,
	compile_ptr_offset,
};
const sv compiler_expr_builtins_str[] = {
	sv_from_null_terminated("if"),
	sv_from_null_terminated("let"),
	sv_from_null_terminated("begin"),
	sv_from_null_terminated("+"),
	sv_from_null_terminated("-"),
	sv_from_null_terminated("<="),
	sv_from_null_terminated("_store"),
	sv_from_null_terminated("_load"),
	sv_from_null_terminated("_sizeof"),
	sv_from_null_terminated("_field_offset"),
	sv_from_null_terminated("_stack_alloc"),
	sv_from_null_terminated("_ptr_offset"),
};
static_assert(ARRAY_LENGTH(compiler_expr_builtins_str) == ARRAY_LENGTH(compiler_expr_builtins));

// (<identifier> <expr>*)
TypeID compile_sexpr(Compiler *compiler, CompilerState *compstate, const AstNode *function_node)
{
	// Get the function name
	const AstNode *identifier_node = ast_get_left_child(&compstate->nodes, function_node);
	const Token *identifier = ast_get_token(&compstate->tokens, identifier_node);
	sv identifier_str = sv_substr(compstate->input.text, identifier->span);

	// Dispatch to the correct builtin compile function (define, iadd, etc)
	const uint32_t compstate_builtin_length = ARRAY_LENGTH(compiler_expr_builtins);
	for (uint32_t i = 0; i < compstate_builtin_length; ++i) {
		if (sv_equals(identifier_str, compiler_expr_builtins_str[i])) {
			return compiler_expr_builtins[i](compiler, compstate, function_node);
		}
	}

	// If the identifier is not a builtin, generate a function call
	Module *current_module = compstate->current_module;
	Function *found_function = nullptr;
	uint32_t i_function = 0;
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
		compstate->error = identifier->span;
		return UNIT_TYPE;
	}

	// Typecheck arguments
	uint32_t i_sig_arg_type = 0;
	uint32_t i_arg_node = identifier_node->right_sibling_index;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_node = ast_get_node(&compstate->nodes, i_arg_node);
		// There is an expr but the signature has ended
		if (i_sig_arg_type >= found_function->arg_count) {
			compstate->result = Result::CompilerUnexpectedExpression;
			SET_RESULT(compstate->result);
			compstate->error = arg_node->span;
			return UNIT_TYPE;
		}

		// compile expr
		TypeID arg_type = compile_expr(compiler, compstate, arg_node);
		if (compstate->result != Result::Ok) {
			return UNIT_TYPE;
		}

		// typecheck
		if (!type_similar(compstate, arg_type, found_function->arg_types[i_sig_arg_type])) {
			compstate->result = Result::CompilerExpectedTypeGot;
			SET_RESULT(compstate->result);
			compstate->error = arg_node->span;
			compstate->expected_type = found_function->arg_types[i_sig_arg_type];
			compstate->got_type = arg_type;
		}

		i_arg_node = arg_node->right_sibling_index;
		i_sig_arg_type += 1;
	}

	// There is an argument left in the signature
	if (i_sig_arg_type < found_function->arg_count) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = function_node->span;
		compstate->expected_type = found_function->arg_types[i_sig_arg_type];
		compstate->got_type = UNIT_TYPE;
		return UNIT_TYPE;
	}

	// The found function signature matched
	compiler_bytecode_call(compiler, compstate, uint8_t(i_function));

	return found_function->return_type;
}

// A module is the "root" of a script, a series of S-expression
void compile_module(Compiler *compiler, CompilerState *compstate)
{
	const AstNode *root_node = vec_at(&compstate->nodes, 0);

	uint32_t i_root_expr = root_node->left_child_index;
	while (ast_is_valid(i_root_expr)) {
		const AstNode *root_expr = ast_get_node(&compstate->nodes, i_root_expr);

		// Validate that a root S-expression starts with a token
		const AstNode *first_sexpr_member = ast_get_left_child(&compstate->nodes, root_expr);
		const bool is_an_atom = ast_has_left_child(root_expr) && ast_is_atom(first_sexpr_member);
		if (!is_an_atom) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			return;
		}
		const Token *atom_token = vec_at(&compstate->tokens, first_sexpr_member->atom_token_index);
		const bool is_an_identifier = atom_token->kind == TokenKind::Identifier;
		if (!is_an_identifier) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			return;
		}

		// Find the compiler builtin for this S-expression
		sv identifier_str = sv_substr(compstate->input.text, atom_token->span);
		uint32_t i_builtin = 0;
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

		// Go to the next root expression
		i_root_expr = root_expr->right_sibling_index;
	}
}

Compiler *compiler_init()
{
	Compiler *compiler = static_cast<Compiler *>(calloc(1, sizeof(Compiler)));
	compiler->modules = vec_init<Module>(8);
	return compiler;
}

void module_init(Module *new_module, sv module_name)
{
	const uint32_t functions_capacity = 16;
	new_module->functions_capacity = functions_capacity;
	new_module->functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));

	const uint32_t bytecode_capacity = 1024;
	new_module->bytecode_capacity = bytecode_capacity;
	new_module->bytecode = static_cast<uint8_t *>(calloc(bytecode_capacity, sizeof(OpCodeKind)));

	const uint32_t types_capacity = 8;
	new_module->types_capacity = types_capacity;
	new_module->types = static_cast<UserDefinedType *>(calloc(types_capacity, sizeof(UserDefinedType)));

	const uint32_t constants_capacity = 16;
	new_module->constant_strings_capacity = constants_capacity;
	new_module->constant_strings = static_cast<sv *>(calloc(constants_capacity, sizeof(sv)));
	new_module->constants_u32_capacity = constants_capacity;
	new_module->constants_u32 = static_cast<uint32_t *>(calloc(constants_capacity, sizeof(uint32_t)));

	new_module->name = module_name;
}

Result compile_module(Compiler *compiler, sv module_name, sv input, Module **out_module)
{
	// Build text input
	TextInput text_input = {};
	text_input.text = input;

	uint32_t line_endings_count = 0;
	for (uint32_t i = 0; i < input.length; ++i) {
		if (input.chars[i] == '\n') {
			line_endings_count += 1;
		}
	}
	text_input.line_endings = vec_init<uint32_t>(line_endings_count);
	text_input.line_endings.length = line_endings_count;
	line_endings_count = 0;
	for (uint32_t i = 0; i < input.length; ++i) {
		if (input.chars[i] == '\n') {
			uint32_t *new_line_ending = vec_at(&text_input.line_endings, line_endings_count);
			*new_line_ending = i;
			line_endings_count += 1;
		}
	}

	// Generate tokens
	Lexer lexer = {};
	lexer.tokens = vec_init<Token>(4096);
	lexer_scan(&lexer, text_input.text);
	if (lexer.result != LexerResult::LexerDone) {
		fprintf(stderr, "# Lexer returned %s\n", Result_str[uint32_t(lexer.result)]);

		uint32_t error_offset = lexer.error.start;
		uint32_t error_line = 0;
		uint32_t error_col = 0;
		text_input_get_line_col(&text_input, error_offset, &error_line, &error_col);

		sv error_str = sv_substr(input, lexer.error);
		fprintf(stderr, "Error at (%u:%u): '%.*s'\n", error_line, error_col, int(error_str.length), error_str.chars);
		return (Result)lexer.result;
	}

	Parser parser = {};
	parser.tokens = lexer.tokens;
	parser.nodes = vec_init<AstNode>(4096);
	parse_module(&parser);

	if (parser.result != ParserResult::Ok) {
		fprintf(stderr,
			"# Parser[token_length: %u, i_current_token: %u] returned %s\n",
			parser.tokens.length,
			parser.i_current_token,
			Result_str[uint32_t(parser.result)]);

		uint32_t error_offset = parser.error.start;
		uint32_t error_line = 0;
		uint32_t error_col = 0;
		text_input_get_line_col(&text_input, error_offset, &error_line, &error_col);
		sv error_str = sv_substr(input, parser.error);
		fprintf(stderr, "Error at (%u:%u): '%.*s'\n", error_line, error_col, int(error_str.length), error_str.chars);

		if (parser.tokens.length > 0) {
			uint32_t i_last_token =
				parser.i_current_token < parser.tokens.length ? parser.i_current_token : parser.tokens.length - 1;
			const Token *last_token = vec_at(&parser.tokens, i_last_token);
			sv last_token_str = sv_substr(text_input.text, last_token->span);

			const char *token_kind_str = TokenKind_str[uint32_t(last_token->kind)];

			fprintf(stderr,
				"# Last seen token is %s[%.*s]\n",
				token_kind_str,
				int(last_token_str.length),
				last_token_str.chars);
		}

		if (parser.expected_token_kind != TokenKind::Invalid) {
			fprintf(stderr, "# Expected token of kind %s\n", TokenKind_str[uint32_t(parser.expected_token_kind)]);
		}

		return (Result)parser.result;
	}

#if 0
	fprintf(stdout, "\nParsing success:\n");
	print_ast(input, parser.ast_nodes);
#endif

	Module new_module = {};
	module_init(&new_module, module_name);

	CompilerState compstate = {};
	compstate.input = text_input;
	compstate.nodes = parser.nodes;
	compstate.tokens = lexer.tokens;
	compstate.current_module = &new_module;
	compstate.scopes = vec_init<LexicalScope>(16);
	compile_module(compiler, &compstate);

	if (compstate.result != Result::Ok) {
		fprintf(stderr,
			"%s:%d:0: error: Compstate[] returned %s\n",
			compstate.result_file,
			compstate.result_file_line,
			Result_str[uint32_t(compstate.result)]);

		uint32_t error_offset = compstate.error.start;
		uint32_t error_line = 0;
		uint32_t error_col = 0;
		text_input_get_line_col(&compstate.input, error_offset, &error_line, &error_col);
		sv error_str = sv_substr(input, compstate.error);
		fprintf(stderr, "Error at (%u:%u): '%.*s'\n", error_line, error_col, int(error_str.length), error_str.chars);

		const char *expected_type_str = "(struct)";
		if (!type_id_is_user_defined(compstate.expected_type)) {
			expected_type_str = BuiltinTypeKind_str[uint32_t(compstate.expected_type.builtin.kind)];
		}
		fprintf(stderr, "# expected type #%u %s\n", compstate.expected_type.raw, expected_type_str);

		const char *got_type_str = "(struct)";
		if (!type_id_is_user_defined(compstate.got_type)) {
			got_type_str = BuiltinTypeKind_str[uint32_t(compstate.got_type.builtin.kind)];
		}
		fprintf(stderr, "# got type #%u %s\n", compstate.got_type.raw, got_type_str);

		return compstate.result;
	}

	fprintf(stdout, "\nCompilation success:\n");
	fprintf(stdout, "Exported types: %u\n", new_module.types_length);
	for (uint32_t offset = 0; offset < new_module.bytecode_length; ++offset) {
		uint8_t opcode = new_module.bytecode[offset];
		if (opcode >= uint8_t(OpCodeKind::Count)) {
			break;
		}

		OpCodeKind opcode_kind = OpCodeKind(opcode);
		fprintf(stdout, "%u\t%s", offset, OpCode_str[uint8_t(opcode_kind)]);

		bool is_unary_u8 = opcode_kind == OpCodeKind::StoreLocal || opcode_kind == OpCodeKind::LoadLocal
		                   || opcode_kind == OpCodeKind::Call || opcode_kind == OpCodeKind::LoadConstantStr
		                   || opcode_kind == OpCodeKind::LoadConstantU32;

		bool is_unary_u32 = opcode_kind == OpCodeKind::Jump || opcode_kind == OpCodeKind::ConditionalJump;

		bool is_unary_sv = opcode_kind == OpCodeKind::DebugLabel;
		bool is_unary_typeid = opcode_kind == OpCodeKind::Store || opcode_kind == OpCodeKind::Load;

		if (is_unary_u8) {
			uint8_t *bytecode_u8 = reinterpret_cast<uint8_t *>(new_module.bytecode + offset + 1);
			fprintf(stdout, " %u", bytecode_u8[0]);
			offset += sizeof(uint8_t);
		} else if (is_unary_u32) {
			uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(new_module.bytecode + offset + 1);
			fprintf(stdout, " %u", bytecode_u32[0]);
			offset += sizeof(uint32_t);
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
		} else if (is_unary_typeid) {
			offset += sizeof(TypeID);

		}

		fprintf(stdout, "\n");
	}

	// Everything went well, copy the new compiled module to the persistant compiler state
	// Find module
	uint32_t i_module = 0;
	for (; i_module < compiler->modules.length; ++i_module) {
		Module *module = vec_at(&compiler->modules, i_module);
		if (sv_equals(module->name, module_name)) {
			break;
		}
	}
	// Not found, create a new module
	if (i_module >= compiler->modules.length) {
		if (i_module >= compiler->modules.capacity) {
			return Result::Fatal;
		}

		i_module = compiler->modules.length;
		vec_append(&compiler->modules, {});
	}

	Module *compiler_module = vec_at(&compiler->modules, i_module);
	// TODO: free previous module?
	*compiler_module = new_module;

	*out_module = vec_at(&compiler->modules, i_module);
	return Result::Ok;
}

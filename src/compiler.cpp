#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "debug.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

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

struct LexicalScope
{
	sv *variables_name;
	TypeID *variables_type;
	uint32_t variables_length;
};

struct Compiler
{
	// persisted
	vec<Module> modules;
	// runtime
	vec<LexicalScope> scopes;
	Module *current_module;
	CompilationUnit *compunit;
};

// bytecode functions

static void compiler_push_opcode(Compiler *compiler, OpCodeKind opcode_kind)
{
	Module *current_module = compiler->current_module;
	if (current_module->bytecode_length + 1 >= current_module->bytecode_capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return;
	}

	current_module->bytecode[current_module->bytecode_length] = uint8_t(opcode_kind);
	current_module->bytecode_length += 1;
}

template <typename T>
static T *compiler_push_scalar(Compiler *compiler, T value)
{
	Module *current_module = compiler->current_module;
	uint32_t to_write = sizeof(T);
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return nullptr;
	}

	T *bytecode = reinterpret_cast<T *>(current_module->bytecode + current_module->bytecode_length);
	bytecode[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode;
}

static TypeID *compiler_push_type_id(Compiler *compiler, TypeID id)
{
	return compiler_push_scalar(compiler, id);
}

static void compiler_push_sv(Compiler *compiler, sv value)
{
	Module *current_module = compiler->current_module;

	uint32_t to_write = uint32_t(sizeof(uint32_t) + value.length * sizeof(char));
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
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

static void compiler_bytecode_load_constant_u32(Compiler *compiler, uint8_t i_constant)
{
	compiler_push_opcode(compiler, OpCodeKind::LoadConstantU32);
	compiler_push_scalar(compiler, i_constant);
}

static void compiler_bytecode_load_constant_str(Compiler *compiler, uint8_t i_constant)
{
	compiler_push_opcode(compiler, OpCodeKind::LoadConstantStr);
	compiler_push_scalar(compiler, i_constant);
}

static void compiler_bytecode_call(Compiler *compiler, uint8_t i_function)
{
	compiler_push_opcode(compiler, OpCodeKind::Call);
	compiler_push_scalar(compiler, i_function);
}

static void compiler_bytecode_call_foreign(Compiler *compiler)
{
	compiler_push_opcode(compiler, OpCodeKind::CallForeign);
}

static void compiler_bytecode_ret(Compiler *compiler)
{
	compiler_push_opcode(compiler, OpCodeKind::Ret);
}

static uint32_t *compiler_bytecode_conditional_jump(Compiler *compiler, uint32_t addr)
{
	compiler_push_opcode(compiler, OpCodeKind::ConditionalJump);
	return compiler_push_scalar(compiler, addr);
}

static uint32_t *compiler_bytecode_jump(Compiler *compiler, uint32_t addr)
{
	compiler_push_opcode(compiler, OpCodeKind::Jump);
	return compiler_push_scalar(compiler, addr);
}

static void compiler_bytecode_store_local(Compiler *compiler, uint8_t i_local)
{
	compiler_push_opcode(compiler, OpCodeKind::StoreLocal);
	compiler_push_scalar(compiler, i_local);
}

static void compiler_bytecode_load_local(Compiler *compiler, uint8_t i_local)
{
	compiler_push_opcode(compiler, OpCodeKind::LoadLocal);
	compiler_push_scalar(compiler, i_local);
}

static void compiler_bytecode_stack_alloc(Compiler *compiler, TypeID pointed_memory_type)
{
	compiler_push_opcode(compiler, OpCodeKind::StackAlloc);
	compiler_push_type_id(compiler, pointed_memory_type);
}

static void compiler_bytecode_load(Compiler *compiler, TypeID expr_type_id)
{
	compiler_push_opcode(compiler, OpCodeKind::Load);
	compiler_push_type_id(compiler, expr_type_id);
}

static void compiler_bytecode_store(Compiler *compiler, TypeID expr_type_id)
{
	compiler_push_opcode(compiler, OpCodeKind::Store);
	compiler_push_type_id(compiler, expr_type_id);
}

static void compiler_bytecode_ptr_offset(Compiler *compiler, TypeID return_type_id)
{
	compiler_push_opcode(compiler, OpCodeKind::PtrOffset);
	compiler_push_type_id(compiler, return_type_id);
}

static void compiler_bytecode_debug_label(Compiler *compiler, sv label)
{
	compiler_push_opcode(compiler, OpCodeKind::DebugLabel);
	compiler_push_sv(compiler, label);
}

template <typename Lambda>
static Function *compiler_compile_function(Compiler *compiler,
	sv function_name,
	TypeID return_type,
	Token *arg_identifiers,
	TypeID *arg_types,
	uint32_t args_length,
	Lambda compile_body_fn)
{
	Module *current_module = compiler->current_module;
	for (uint32_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, function_name)) {
			// Actually it should be possible to recompile a function if signature has not changed.
			INIT_ERROR(&compiler->compunit->error, ErrorCode::DuplicateSymbol);
			return nullptr;
		}
	}

	if (current_module->functions_length + 1 > current_module->functions_capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
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
	compiler_bytecode_debug_label(compiler, function_name);
	// Create a variable scope
	compiler_push_scope(compiler);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		compiler_bytecode_store_local(compiler, uint8_t(args_length - 1 - i_arg));
	}

	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		TypeID arg_type = arg_types[i_arg];
		function->arg_types[function->arg_count] = arg_types[i_arg];
		function->arg_count += 1;

		uint32_t i_variable = 0;
		if (!compiler_push_variable(compiler, &arg_identifiers[i_arg], arg_type, &i_variable)) {
			return nullptr;
		}
	}

	// <-- Compile the body
	TypeID body_type = compile_body_fn();

	compiler_pop_scope(compiler);
	compiler_bytecode_ret(compiler);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return nullptr;
	}

	bool valid_return_type = type_similar(return_type, body_type);
	if (!valid_return_type) {
		printf("%.*s\n", int(function->name.length), function->name.chars);
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.expected_type = return_type;
		compiler->compunit->error.got_type = body_type;
	}

	return function;
}

uint32_t type_get_size(Module *module, TypeID id)
{
	if (id.builtin.is_user_defined != 0) {
		if (id.user_defined.index >= module->types_length) {
			return ~uint32_t(0);
		}
		return module->types[id.user_defined.index].size;
	} else {
		return BuiltinTypeKind_size[uint32_t(id.builtin.kind)];
	}
}

static constexpr uint32_t SCOPE_MAX_VARIABLES = 16;

void compiler_push_scope(Compiler *compiler)
{
	if (compiler->scopes.length >= compiler->scopes.capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return;
	}

	LexicalScope new_scope = {};
	new_scope.variables_name = (sv *)calloc(SCOPE_MAX_VARIABLES, sizeof(sv));
	new_scope.variables_type = (TypeID *)calloc(SCOPE_MAX_VARIABLES, sizeof(TypeID));
	vec_append(&compiler->scopes, new_scope);
}

void compiler_pop_scope(Compiler *compiler)
{
	if (compiler->scopes.length == 0) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return;
	}

	uint32_t last_scope_index = compiler->scopes.length - 1;
	LexicalScope *current_scope = vec_at(&compiler->scopes, last_scope_index);
	free(current_scope->variables_name);
	free(current_scope->variables_type);
	vec_swap_remove(&compiler->scopes, last_scope_index);
}

bool compiler_push_variable(Compiler *compiler, const Token *identifier_token, TypeID type, uint32_t *i_variable_out)
{
	if (compiler->scopes.length >= compiler->scopes.capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return false;
	}

	uint32_t last_scope_index = compiler->scopes.length - 1;
	LexicalScope *current_scope = vec_at(&compiler->scopes, last_scope_index);
	if (current_scope->variables_length >= SCOPE_MAX_VARIABLES) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return false;
	}

	uint32_t i_new_variable = current_scope->variables_length;
	current_scope->variables_length += 1;

	sv name_str = sv_substr(compiler->compunit->input, identifier_token->span);

	current_scope->variables_name[i_new_variable] = name_str;
	current_scope->variables_type[i_new_variable] = type;

	*i_variable_out = i_new_variable;
	return true;
}

bool compiler_lookup_variable(
	Compiler *compiler, const Token *identifier_token, TypeID *type_out, uint32_t *i_variable_out)
{
	if (compiler->scopes.length == 0) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return false;
	}

	sv tofind_name = sv_substr(compiler->compunit->input, identifier_token->span);

	for (uint32_t i_scope = compiler->scopes.length - 1; i_scope < compiler->scopes.length; --i_scope) {
		LexicalScope *scope = vec_at(&compiler->scopes, i_scope);

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
TypeID compile_sexpr(Compiler *compiler, const AstNode *node);

//

static uint32_t compile_find_or_add_constant(Compiler *compiler, uint32_t value)
{
	// Find or add the number literal to the constant pool
	// TODO: It's slow.
	uint32_t i_constant = 0;
	uint32_t constants_u32_length = compiler->current_module->constants_u32_length;
	for (; i_constant < constants_u32_length; ++i_constant) {
		uint32_t constant_u32 = compiler->current_module->constants_u32[i_constant];
		if (value == constant_u32) {
			break;
		}
	}

	// The constant string was not found, add it.
	if (i_constant == constants_u32_length) {
		if (constants_u32_length >= compiler->current_module->constants_u32_capacity) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
			return INVALID_NODE_INDEX;
		}
		compiler->current_module->constants_u32_length += 1;
		compiler->current_module->constants_u32[i_constant] = value;
	}
	return i_constant;
}

static TypeID compile_atom(Compiler *compiler, const Token *token)
{
	sv token_sv = sv_substr(compiler->compunit->input, token->span);

	if (token->kind == TokenKind::Identifier) {
		// Refer a declared variable
		// <identifier>
		TypeID ty = UNIT_TYPE;
		uint32_t i_variable = 0;
		if (!compiler_lookup_variable(compiler, token, &ty, &i_variable)) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::UnknownSymbol);
			compiler->compunit->error.span = token->span;
			return UNIT_TYPE;
		}
		compiler_bytecode_load_local(compiler, uint8_t(i_variable));
		return ty;
	} else if (token->kind == TokenKind::Number) {
		// An integer constant
		// <number>
		int32_t token_number = sv_to_int(token_sv);
		uint32_t i_constant = compile_find_or_add_constant(compiler, uint32_t(token_number));
		compiler_bytecode_load_constant_u32(compiler, uint8_t(i_constant));
		return type_id_new_builtin(BuiltinTypeKind::Int);
	} else if (token->kind == TokenKind::StringLiteral) {
		// A string literal
		// "str"
		sv value_sv = sv_substr(token_sv, span{1, uint32_t(token_sv.length) - 2});

		// Find or add the string literal to the constant pool
		// TODO: It's slow.
		uint32_t i_constant = 0;
		uint32_t constant_strings_length = compiler->current_module->constant_strings_length;
		for (; i_constant < constant_strings_length; ++i_constant) {
			sv constant_sv = compiler->current_module->constant_strings[i_constant];
			if (sv_equals(value_sv, constant_sv)) {
				break;
			}
		}
		// The constant string was not found, add it.
		if (i_constant == constant_strings_length) {
			if (constant_strings_length >= compiler->current_module->constant_strings_capacity) {
				INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
				return UNIT_TYPE;
			}
			compiler->current_module->constant_strings_length += 1;
			compiler->current_module->constant_strings[i_constant] = value_sv;
		}

		compiler_bytecode_load_constant_str(compiler, uint8_t(i_constant));

		TypeID new_type_id = type_id_new_builtin(BuiltinTypeKind::Str);
		return new_type_id;
	} else {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return UNIT_TYPE;
	}
}

// <identifier> | <number> | <s-expression>
TypeID compile_expr(Compiler *compiler, const AstNode *node)
{
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	if (ast_is_atom(node)) {
		const Token *token = ast_get_token(&compiler->compunit->tokens, node);
		TypeID return_type = compile_atom(compiler, token);
		return return_type;
	} else if (ast_has_left_child(node)) {
		TypeID return_type = compile_sexpr(compiler, node);
		return return_type;

	} else {
		// () unit value
		return UNIT_TYPE;
	}
}

TypeID compile_sexprs_return_last(Compiler *compiler, const AstNode *node)
{
	const AstNode *first_expr_node = node;
	TypeID return_type = compile_expr(compiler, first_expr_node);

	uint32_t i_next_expr_node = first_expr_node->right_sibling_index;
	while (ast_is_valid(i_next_expr_node)) {
		const AstNode *next_expr_node = ast_get_node(&compiler->compunit->nodes, i_next_expr_node);
		return_type = compile_expr(compiler, next_expr_node);
		i_next_expr_node = next_expr_node->right_sibling_index;
	}

	return return_type;
}

// Defines a new function
// (define (<name> <return_type>) (<args>*) <expression>+)
TypeID compile_define(Compiler *compiler, const AstNode *node)
{
	DefineNode define_node = {};
	parse_define_sig(compiler->compunit, node, &define_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}
	parse_define_body(compiler->compunit, node, &define_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	sv function_name_token_str = sv_substr(compiler->compunit->input, define_node.function_name_token->span);

	// -- Type checking
	TypeID return_type = parse_type(compiler->compunit, compiler->current_module, define_node.return_type_node);
	TypeID arg_types[MAX_ARGUMENTS] = {};
	for (uint32_t i_arg = 0; i_arg < define_node.args_length; ++i_arg) {
		arg_types[i_arg] = parse_type(compiler->compunit, compiler->current_module, define_node.arg_nodes[i_arg]);
	}

	Function *new_function = compiler_compile_function(compiler,
		function_name_token_str,
		return_type,
		define_node.arg_identifiers,
		arg_types,
		define_node.args_length,
		[&]() -> TypeID {
			// <-- Compile the body
			TypeID body_type = compile_sexprs_return_last(compiler, define_node.body_node);
			return body_type;
		});

	if (new_function == nullptr) {
		return UNIT_TYPE;
	}
	return new_function->return_type;
}

// Defines a new foreign function
// (define-foreign (<name> <return_type>) (<args>))
TypeID compile_define_foreign(Compiler *compiler, const AstNode *node)
{
	DefineNode nodes = {};
	parse_define_sig(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}
	sv function_name_token_str = sv_substr(compiler->compunit->input, nodes.function_name_token->span);

	// -- Type checking
	Module *current_module = compiler->current_module;
	for (uint32_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (sv_equals(function->name, function_name_token_str)) {
			// Actually it should be possible to recompile a function if signature has not changed.
			INIT_ERROR(&compiler->compunit->error, ErrorCode::DuplicateSymbol);
			compiler->compunit->error.span = node->span;
			return UNIT_TYPE;
		}
	}
	if (current_module->functions_length + 1 >= current_module->functions_capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	// Create a new function symbol
	Function *function = current_module->functions + current_module->functions_length;
	function->name = function_name_token_str;
	function->address = current_module->bytecode_length;
	function->is_foreign = true;

	current_module->functions_length += 1;

	// -- Compiling
	TypeID return_type = parse_type(compiler->compunit, compiler->current_module, nodes.return_type_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}
	function->return_type = return_type;

	// Add a debug label to identify functions easily in the bytecode
	compiler_bytecode_debug_label(compiler, function_name_token_str);
	// Create a variable scope
	compiler_push_scope(compiler);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint32_t i_arg = 0; i_arg < nodes.args_length; ++i_arg) {
		compiler_bytecode_store_local(compiler, uint8_t(nodes.args_length - 1 - i_arg));
	}
	// push argument variables
	for (uint32_t i_arg = 0; i_arg < nodes.args_length; ++i_arg) {
		TypeID arg_type = parse_type(compiler->compunit, compiler->current_module, nodes.arg_nodes[i_arg]);
		function->arg_types[function->arg_count] = arg_type;
		function->arg_count += 1;

		uint32_t i_variable = 0;
		if (!compiler_push_variable(compiler, &nodes.arg_identifiers[i_arg], arg_type, &i_variable)) {
			return UNIT_TYPE;
		}
	}
	compiler_bytecode_call_foreign(compiler);
	compiler_pop_scope(compiler);
	compiler_bytecode_ret(compiler);
	return return_type;
}

// Defines a new struct
TypeID compile_struct(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	StructNode nodes = {};
	parse_struct(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}
	sv struct_name_token_str = sv_substr(compiler->compunit->input, nodes.struct_name_token->span);

	// -- Type checking
	// Check if the type is already defined
	for (uint32_t i_type = 0; i_type < compiler->current_module->types_length; ++i_type) {
		UserDefinedType *type = compiler->current_module->types + i_type;
		if (sv_equals(type->name, struct_name_token_str)) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::DuplicateSymbol);
			compiler->compunit->error.span = node->span;
			// Actually it should be possible to recompile a function if signature has not changed.
			return UNIT_TYPE;
		}
	}
	// Not enough space to add a type
	if (compiler->current_module->types_length + 1 >= compiler->current_module->types_capacity) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	// -- Create a new structure type
	TypeID fields_type[MAX_STRUCT_FIELDS] = {};

	TypeID struct_type_id = type_id_new_user_defined(compiler->current_module->types_length);
	compiler->current_module->types_length += 1;

	UserDefinedType *struct_type = compiler->current_module->types + struct_type_id.user_defined.index;
	*struct_type = {};
	struct_type->size = 0;
	struct_type->name = struct_name_token_str;

	uint32_t struct_size = 0;
	for (uint32_t i_field = 0; i_field < nodes.fields_length; ++i_field) {
		TypeID field_type = parse_type(compiler->compunit, compiler->current_module, nodes.field_type_nodes[i_field]);
		if (compiler->compunit->error.code != ErrorCode::Ok) {
			return UNIT_TYPE;
		}

		struct_type->field_types[i_field] = field_type;
		struct_type->field_names[i_field] = sv_substr(compiler->compunit->input, nodes.field_identifiers[i_field].span);
		struct_type->field_offsets[i_field] = struct_size;

		struct_size += type_get_size(compiler->current_module, field_type);
		fields_type[i_field] = field_type;
	}

	struct_type->field_count = nodes.fields_length;
	struct_type->size = struct_size;

	return struct_type_id;
}

// Conditional branch
TypeID compile_if(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	IfNode nodes = {};
	parse_if(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	// Compile the condition first,
	/*TypeID cond_expr =*/compile_expr(compiler, nodes.cond_expr_node);
	// TODO: no type checking?
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	// If true, jump to the true branch (patch the jump adress later)
	uint32_t *jump_to_true_branch = compiler_bytecode_conditional_jump(compiler, 0);

	// Then compile the else branch, because the condition was false
	TypeID else_expr = compile_expr(compiler, nodes.else_expr_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	// Jump over the true branch (patch the jump adress later)
	uint32_t *jump_to_end = compiler_bytecode_jump(compiler, 0);

	// Compile the true branch
	const uint32_t then_branch_address = compiler->current_module->bytecode_length;
	TypeID then_expr = compile_expr(compiler, nodes.then_expr_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	const uint32_t end_address = compiler->current_module->bytecode_length;
	*jump_to_true_branch = uint32_t(then_branch_address);
	*jump_to_end = uint32_t(end_address);

	bool valid_return_type = type_similar(then_expr, else_expr);
	if (!valid_return_type) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.span = node->span;
		compiler->compunit->error.expected_type = then_expr;
		compiler->compunit->error.got_type = else_expr;
	}

	return then_expr;
}

TypeID compile_let(Compiler *compiler, const AstNode *node)
{
	LetNode nodes = {};
	parse_let(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	TypeID expr_type = compile_expr(compiler, nodes.value_node);
	uint32_t i_variable = 0;
	if (!compiler_push_variable(compiler, nodes.name_token, expr_type, &i_variable)) {
		return UNIT_TYPE;
	}

	compiler_bytecode_store_local(compiler, uint8_t(i_variable));
	return UNIT_TYPE;
}

// (begin <expr1> <expr2> ...)
TypeID compile_begin(Compiler *compiler, const AstNode *node)
{
	const AstNode *begin_node = ast_get_left_child(&compiler->compunit->nodes, node);
	// A begin expression must have at least one expr
	if (!ast_has_right_sibling(begin_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedExpr);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	const AstNode *first_sexpr = ast_get_right_sibling(&compiler->compunit->nodes, begin_node);
	return compile_sexprs_return_last(compiler, first_sexpr);
}

// (<op> <lhs> <rhs>)
TypeID compile_binary_opcode(Compiler *compiler, const AstNode *node, TypeID type, OpCodeKind opcode)
{
	// -- Parsing
	BinaryOpNode nodes = {};
	parse_binary_op(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	// -- Type checking
	TypeID lhs = compile_expr(compiler, nodes.lhs_node);
	TypeID rhs = compile_expr(compiler, nodes.rhs_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	bool lhs_valid = type_similar(lhs, type);
	bool rhs_valid = type_similar(rhs, type);

	if (!lhs_valid) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.expected_type = type;
		compiler->compunit->error.got_type = lhs;
		compiler->compunit->error.span = nodes.lhs_node->span;
		return UNIT_TYPE;
	}

	if (!rhs_valid) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.expected_type = type;
		compiler->compunit->error.got_type = rhs;
		compiler->compunit->error.span = nodes.rhs_node->span;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, opcode);

	return lhs;
}

// Add two integers
TypeID compile_iadd(Compiler *compiler, const AstNode *node)
{
	TypeID int_type_id = type_id_new_builtin(BuiltinTypeKind::Int);
	return compile_binary_opcode(compiler, node, int_type_id, OpCodeKind::IAdd);
}

// Substract two integers
TypeID compile_isub(Compiler *compiler, const AstNode *node)
{
	TypeID int_type_id = type_id_new_builtin(BuiltinTypeKind::Int);
	return compile_binary_opcode(compiler, node, int_type_id, OpCodeKind::ISub);
}

// Compare two integers
// (<= <lhs> <rhs>)
TypeID compile_ltethan(Compiler *compiler, const AstNode *node)
{
	TypeID int_type_id = type_id_new_builtin(BuiltinTypeKind::Int);
	return compile_binary_opcode(compiler, node, int_type_id, OpCodeKind::ILessThanEq);
}

// Load a value at the given memory address
// (load <addr>)
TypeID compile_load(Compiler *compiler, const AstNode *node)
{
	UnaryOpNode nodes = {};
	parse_unary_op(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	TypeID addr_type_id = compile_expr(compiler, nodes.value_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	// Typecheck
	if (!type_id_is_pointer(addr_type_id)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.span = nodes.value_node->span;
		compiler->compunit->error.expected_type = {};
		compiler->compunit->error.got_type = addr_type_id;
		return UNIT_TYPE;
	}

	TypeID pointed_type_id = type_id_deref_pointer(addr_type_id);
	compiler_bytecode_load(compiler, pointed_type_id);
	return pointed_type_id;
}

// Store a value at the given memory address
// (store <addr> <value>)
TypeID compile_store(Compiler *compiler, const AstNode *node)
{
	BinaryOpNode nodes = {};
	parse_binary_op(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	TypeID addr_type_id = compile_expr(compiler, nodes.lhs_node);
	TypeID expr_type_id = compile_expr(compiler, nodes.rhs_node);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	// Typecheck
	TypeID expected_pointer_type = type_id_pointer_from(expr_type_id);
	if (!type_similar(expected_pointer_type, addr_type_id)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.span = nodes.lhs_node->span;
		compiler->compunit->error.expected_type = expected_pointer_type;
		compiler->compunit->error.got_type = addr_type_id;
		return UNIT_TYPE;
	}

	compiler_bytecode_store(compiler, expr_type_id);
	return UNIT_TYPE;
}

// Returns the size of a type
TypeID compile_sizeof(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	UnaryOpNode nodes = {};
	parse_unary_op(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	TypeID expr_type = parse_type(compiler->compunit, compiler->current_module, nodes.value_node);
	uint32_t type_size = type_get_size(compiler->current_module, expr_type);
	uint32_t i_constant = compile_find_or_add_constant(compiler, type_size);
	compiler_bytecode_load_constant_u32(compiler, uint8_t(i_constant));
	return type_id_new_builtin(BuiltinTypeKind::Int);
}

TypeID compile_field_offset(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	BinaryOpNode nodes = {};
	parse_binary_op(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	// -- Typecheck
	TypeID expr_type = parse_type(compiler->compunit, compiler->current_module, nodes.lhs_node);
	if (!type_id_is_user_defined(expr_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.span = nodes.lhs_node->span;
		compiler->compunit->error.got_type = expr_type;
		return UNIT_TYPE;
	}

	bool is_an_identifier = ast_is_atom(nodes.rhs_node);
	const Token *field_token = vec_at(&compiler->compunit->tokens, nodes.rhs_node->atom_token_index);
	is_an_identifier = is_an_identifier && field_token->kind == TokenKind::Identifier;
	if (!is_an_identifier) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedIdentifier);
		compiler->compunit->error.span = nodes.rhs_node->span;
		return UNIT_TYPE;
	}
	sv field_identifier_str = sv_substr(compiler->compunit->input, field_token->span);

	// -- Find field offset
	if (expr_type.user_defined.index >= compiler->current_module->types_length) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::Fatal);
		return UNIT_TYPE;
	}

	UserDefinedType *type = compiler->current_module->types + expr_type.user_defined.index;
	for (uint32_t i_field = 0; i_field < type->field_count; ++i_field) {
		if (sv_equals(type->field_names[i_field], field_identifier_str)) {
			uint32_t i_constant = compile_find_or_add_constant(compiler, type->field_offsets[i_field]);
			compiler_bytecode_load_constant_u32(compiler, uint8_t(i_constant));
			return type_id_new_builtin(BuiltinTypeKind::Int);
		}
	}

	INIT_ERROR(&compiler->compunit->error, ErrorCode::UnknownSymbol);
	compiler->compunit->error.span = field_token->span;
	return UNIT_TYPE;
}

// Allocate memory on the stack (_stack_alloc <type> <size>)
TypeID compile_stack_alloc(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	BinaryOpNode nodes = {};
	parse_binary_op(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	TypeID type_of_pointed_memory = parse_type(compiler->compunit, compiler->current_module, nodes.lhs_node);

	/*TypeID size_type =*/compile_expr(compiler, nodes.rhs_node);

	// TODO: We need to bound check the alloc, <size> > sizeof(<type>

	compiler_bytecode_stack_alloc(compiler, type_of_pointed_memory);
	return type_id_pointer_from(type_of_pointed_memory);
}

TypeID compile_ptr_offset(Compiler *compiler, const AstNode *node)
{
	PtrOffsetNodes nodes = {};
	parse_ptr_offset(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode::Ok) {
		return UNIT_TYPE;
	}

	TypeID return_type = parse_type(compiler->compunit, compiler->current_module, nodes.return_type_node);
	if (!type_id_is_pointer(return_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot)
		compiler->compunit->error.got_type = return_type;
		compiler->compunit->error.span = nodes.return_type_node->span;
		return UNIT_TYPE;
	}

	/*TypeID base_pointer_type =*/compile_expr(compiler, nodes.base_pointer_node);

	// Check that the provided offset is an int
	TypeID offset_type = compile_expr(compiler, nodes.offset_node);
	TypeID expected_offset_type = type_id_new_builtin(BuiltinTypeKind::Int);
	if (!type_similar(offset_type, expected_offset_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
		compiler->compunit->error.span = nodes.offset_node->span;
		compiler->compunit->error.got_type = offset_type;
		compiler->compunit->error.expected_type = expected_offset_type;
		return UNIT_TYPE;
	}

	compiler_bytecode_ptr_offset(compiler, return_type);
	return return_type;
}

using CompilerBuiltin = TypeID (*)(Compiler *, const AstNode *);

// There are two kinds of "builtins", the ones allowed at top-level and the other ones
const CompilerBuiltin compiler_top_builtins[] = {
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

const CompilerBuiltin compiler_expr_builtins[] = {
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
TypeID compile_sexpr(Compiler *compiler, const AstNode *function_node)
{
	// Get the function name
	const AstNode *identifier_node = ast_get_left_child(&compiler->compunit->nodes, function_node);
	const Token *identifier = ast_get_token(&compiler->compunit->tokens, identifier_node);
	sv identifier_str = sv_substr(compiler->compunit->input, identifier->span);

	// Dispatch to the correct builtin compile function (define, iadd, etc)
	const uint32_t compiler_builtin_length = ARRAY_LENGTH(compiler_expr_builtins);
	for (uint32_t i = 0; i < compiler_builtin_length; ++i) {
		if (sv_equals(identifier_str, compiler_expr_builtins_str[i])) {
			return compiler_expr_builtins[i](compiler, function_node);
		}
	}

	// If the identifier is not a builtin, generate a function call
	Module *current_module = compiler->current_module;
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
		INIT_ERROR(&compiler->compunit->error, ErrorCode::UnknownSymbol);
		compiler->compunit->error.span = identifier->span;
		return UNIT_TYPE;
	}

	// Typecheck arguments
	uint32_t i_sig_arg_type = 0;
	uint32_t i_arg_node = identifier_node->right_sibling_index;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_node = ast_get_node(&compiler->compunit->nodes, i_arg_node);
		// There is an expr but the signature has ended
		if (i_sig_arg_type >= found_function->arg_count) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::UnexpectedExpression);
			compiler->compunit->error.span = arg_node->span;
			return UNIT_TYPE;
		}

		// compile expr
		TypeID arg_type = compile_expr(compiler, arg_node);
		if (compiler->compunit->error.code != ErrorCode::Ok) {
			return UNIT_TYPE;
		}

		// typecheck
		if (!type_similar(arg_type, found_function->arg_types[i_sig_arg_type])) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedTypeGot);
			compiler->compunit->error.span = arg_node->span;
			compiler->compunit->error.expected_type = found_function->arg_types[i_sig_arg_type];
			compiler->compunit->error.got_type = arg_type;
		}

		i_arg_node = arg_node->right_sibling_index;
		i_sig_arg_type += 1;
	}

	// There is an argument left in the signature
	if (i_sig_arg_type < found_function->arg_count) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedExpr);
		compiler->compunit->error.span = function_node->span;
		compiler->compunit->error.expected_type = found_function->arg_types[i_sig_arg_type];
		compiler->compunit->error.got_type = UNIT_TYPE;
		return UNIT_TYPE;
	}

	// The found function signature matched
	compiler_bytecode_call(compiler, uint8_t(i_function));

	return found_function->return_type;
}

// A module is the "root" of a script, a series of S-expression
void compile_module(Compiler *compiler)
{
	const AstNode *root_node = vec_at(&compiler->compunit->nodes, 0);

	uint32_t i_root_expr = root_node->left_child_index;
	while (ast_is_valid(i_root_expr)) {
		const AstNode *root_expr = ast_get_node(&compiler->compunit->nodes, i_root_expr);

		// Validate that a root S-expression starts with a token
		const AstNode *first_sexpr_member = ast_get_left_child(&compiler->compunit->nodes, root_expr);
		const bool is_an_atom = ast_has_left_child(root_expr) && ast_is_atom(first_sexpr_member);
		if (!is_an_atom) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedIdentifier);
			return;
		}
		const Token *atom_token = vec_at(&compiler->compunit->tokens, first_sexpr_member->atom_token_index);
		const bool is_an_identifier = atom_token->kind == TokenKind::Identifier;
		if (!is_an_identifier) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::ExpectedIdentifier);
			return;
		}

		// Find the compiler builtin for this S-expression
		sv identifier_str = sv_substr(compiler->compunit->input, atom_token->span);
		uint32_t i_builtin = 0;
		for (; i_builtin < ARRAY_LENGTH(compiler_top_builtins); ++i_builtin) {
			if (sv_equals(identifier_str, compiler_top_builtins_str[i_builtin])) {
				compiler_top_builtins[i_builtin](compiler, root_expr);
				break;
			}
		}
		if (i_builtin >= ARRAY_LENGTH(compiler_top_builtins)) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode::UnknownSymbol);
			compiler->compunit->error.span = first_sexpr_member->span;
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

Error compile_module(Compiler *compiler, sv module_name, sv input, Module **out_module)
{
	CompilationUnit compunit = {};
	compunit.input = input;
	compunit.tokens = vec_init<Token>(4096);
	compunit.nodes = vec_init<AstNode>(4096);

	// Generate tokens
	lexer_scan(&compunit);
	if (compunit.error.code != ErrorCode::LexerDone) {
		fprintf(stderr, "# Lexer returned %u\n", uint32_t(compunit.error.code));

		sv error_str = sv_substr(input, compunit.error.span);
		fprintf(stderr, "Error at: '%.*s'\n", int(error_str.length), error_str.chars);
		return compunit.error;
	}

	// Reset the result to Ok from LexerDone
	// TODO: remove lexerdone?
	compunit.error.code = ErrorCode::Ok;

	Parser parser = {};
	parser.compunit = &compunit;
	parse_module(&parser);

	if (compunit.error.code != ErrorCode::Ok) {
		fprintf(stderr,
			"# Parser[token_length: %u, i_current_token: %u] returned %u\n",
			compunit.tokens.length,
			parser.i_current_token,
			uint32_t(compunit.error.code));

		sv error_str = sv_substr(input, compunit.error.span);
		fprintf(stderr, "Error at: '%.*s'\n", int(error_str.length), error_str.chars);

		if (compunit.tokens.length > 0) {
			uint32_t i_last_token =
				parser.i_current_token < compunit.tokens.length ? parser.i_current_token : compunit.tokens.length - 1;
			const Token *last_token = vec_at(&compunit.tokens, i_last_token);
			sv last_token_str = sv_substr(compunit.input, last_token->span);

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

		return compunit.error;
	}

#if 0
	fprintf(stdout, "\nParsing success:\n");
	print_ast(compunit.input, &compunit.tokens, &compunit.nodes, 0);
#endif

	Module new_module = {};
	module_init(&new_module, module_name);

	compiler->compunit = &compunit;
	compiler->current_module = &new_module;
	compiler->scopes = vec_init<LexicalScope>(16);
	compile_module(compiler);

	if (compunit.error.code != ErrorCode::Ok) {
		Error err = compunit.error;

		fprintf(stderr, "%s:%d:0: error: Compiler[] returned %u\n", err.file.chars, err.line, uint32_t(err.code));

		sv error_str = sv_substr(input, err.span);
		fprintf(stderr, "Error at: '%.*s'\n", int(error_str.length), error_str.chars);

		const char *expected_type_str = "(struct)";
		if (!type_id_is_user_defined(err.expected_type)) {
			expected_type_str = BuiltinTypeKind_str[uint32_t(err.expected_type.builtin.kind)];
		}
		fprintf(stderr, "# expected type #%u %s\n", err.expected_type.raw, expected_type_str);

		const char *got_type_str = "(struct)";
		if (!type_id_is_user_defined(err.got_type)) {
			got_type_str = BuiltinTypeKind_str[uint32_t(err.got_type.builtin.kind)];
		}
		fprintf(stderr, "# got type #%u %s\n", err.got_type.raw, got_type_str);

		return err;
	}

	fprintf(stdout, "\nCompilation success:\n");
	fprintf(stdout, "Exported types: %u\n", new_module.types_length);
	print_bytecode(new_module.bytecode, new_module.bytecode_length);

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
			INIT_ERROR(&compunit.error, ErrorCode::Fatal);
			return compunit.error;
		}

		i_module = compiler->modules.length;
		vec_append(&compiler->modules, {});
	}

	Module *compiler_module = vec_at(&compiler->modules, i_module);
	// TODO: free previous module?
	*compiler_module = new_module;

	*out_module = vec_at(&compiler->modules, i_module);
	return compunit.error;
}

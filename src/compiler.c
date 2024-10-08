#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "vm.h"
#include "opcodes.h"

#include <stdint.h>

// bytecode functions

static void compiler_push_opcode(Compiler *compiler, OpCode opcode)
{
	CompilerModule *current_module = &compiler->module;
	if (current_module->bytecode_length + 1 >= ARRAY_LENGTH(current_module->bytecode)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return;
	}

	current_module->bytecode[current_module->bytecode_length] = (uint8_t)(opcode);
	current_module->bytecode_length += 1;
}

static uint8_t *compiler_push_u8(Compiler *compiler, uint8_t value)
{
	CompilerModule *current_module = &compiler->module;
	uint32_t to_write = sizeof(uint8_t);
	if (current_module->bytecode_length + to_write >= ARRAY_LENGTH(current_module->bytecode)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return NULL;
	}

	uint8_t *bytecode = (uint8_t *)(current_module->bytecode + current_module->bytecode_length);
	bytecode[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode;
}

static uint32_t *compiler_push_u32(Compiler *compiler, uint32_t value)
{
	CompilerModule *current_module = &compiler->module;
	uint32_t to_write = sizeof(uint32_t);
	if (current_module->bytecode_length + to_write >= ARRAY_LENGTH(current_module->bytecode)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return NULL;
	}

	uint32_t *bytecode = (uint32_t *)(current_module->bytecode + current_module->bytecode_length);
	bytecode[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode;
}

static uint32_t compiler_push_str(Compiler *compiler, sv value)
{
	CompilerModule *current_module = &compiler->module;
	uint32_t to_write = sizeof(value.length) + value.length;
	if (current_module->constants_length + to_write >= ARRAY_LENGTH(current_module->constants)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return 0;
	}

	uint32_t offset = current_module->constants_length;
	uint8_t *constants_memory = current_module->constants + offset;

	*(uint32_t*)constants_memory = value.length;
	constants_memory += sizeof(value.length);
	
	for (uint32_t i = 0; i < value.length; ++i) {
		constants_memory[i] = (uint8_t)value.chars[i];
	}
	current_module->constants_length += to_write;
	return offset;
}

static int32_t *compiler_push_i32(Compiler *compiler, int32_t value)
{
	CompilerModule *current_module = &compiler->module;
	uint32_t to_write = sizeof(int32_t);
	if (current_module->bytecode_length + to_write >= ARRAY_LENGTH(current_module->bytecode)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return NULL;
	}

	int32_t *bytecode = (int32_t *)(current_module->bytecode + current_module->bytecode_length);
	bytecode[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode;
}

static void compiler_push_sv(Compiler *compiler, sv value)
{
	CompilerModule *current_module = &compiler->module;

	uint32_t to_write = (uint32_t)(sizeof(uint32_t) + value.length * sizeof(char));
	if (current_module->bytecode_length + to_write >= ARRAY_LENGTH(current_module->bytecode)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return;
	}

	uint32_t *bytecode_u32 = (uint32_t*)(current_module->bytecode + current_module->bytecode_length);
	bytecode_u32[0] = (uint32_t)(value.length);

	uint8_t *bytecode_u8 =
		(uint8_t*)(current_module->bytecode + current_module->bytecode_length + sizeof(uint32_t));
	for (uint32_t i = 0; i < value.length; ++i)
		bytecode_u8[i] = (uint8_t)(value.chars[i]);
	current_module->bytecode_length += to_write;
}

static float *compiler_push_f32(Compiler *compiler, float value)
{
	CompilerModule *current_module = &compiler->module;
	uint32_t to_write = sizeof(float);
	if (current_module->bytecode_length + to_write >= ARRAY_LENGTH(current_module->bytecode)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return NULL;
	}

	float *bytecode = (float *)(current_module->bytecode + current_module->bytecode_length);
	bytecode[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode;
}

static uint32_t *compiler_bytecode_push_u32(Compiler *compiler, uint32_t value)
{
	compiler_push_opcode(compiler, OpCode_PushU32);
	return compiler_push_u32(compiler, value);
}

static void compiler_bytecode_push_str(Compiler *compiler, sv value)
{
	uint32_t constants_offset = compiler_push_str(compiler, value);
	compiler_push_opcode(compiler, OpCode_PushStr);
	compiler_push_u32(compiler, constants_offset);
}

static void compiler_bytecode_push_f32(Compiler *compiler, float value)
{
	compiler_push_opcode(compiler, OpCode_PushF32);
	compiler_push_f32(compiler, value);
}

static void compiler_bytecode_push_i32(Compiler *compiler, int32_t value)
{
	compiler_push_opcode(compiler, OpCode_PushU32);
	compiler_push_i32(compiler, value);
}

static void compiler_bytecode_call(Compiler *compiler, uint32_t address, uint8_t num_args)
{
	compiler_push_opcode(compiler, OpCode_Call);
	compiler_push_u32(compiler, address);
	compiler_push_u8(compiler, num_args);
}

static void compiler_bytecode_call_external(Compiler *compiler, uint8_t i_imported_function, uint8_t num_args)
{
	compiler_push_opcode(compiler, OpCode_CallInModule);
	compiler_push_u8(compiler, i_imported_function);
	compiler_push_u8(compiler, num_args);
}

static void compiler_bytecode_call_foreign(Compiler *compiler, uint8_t i_foreign_function, uint8_t num_args)
{
	compiler_push_opcode(compiler, OpCode_CallForeign);
	compiler_push_u8(compiler, i_foreign_function);
	compiler_push_u8(compiler, num_args);
}

static uint32_t *compiler_bytecode_conditional_jump(Compiler *compiler, uint32_t address)
{
	compiler_push_opcode(compiler, OpCode_ConditionalJump);
	return compiler_push_u32(compiler, address);
}

static uint32_t *compiler_bytecode_jump(Compiler *compiler, uint32_t address)
{
	compiler_push_opcode(compiler, OpCode_Jump);
	return compiler_push_u32(compiler, address);
}

static void compiler_bytecode_store_arg(Compiler *compiler, uint8_t i_arg)
{
	compiler_push_opcode(compiler, OpCode_StoreArg);
	compiler_push_u8(compiler, i_arg);
}

static void compiler_bytecode_load_arg(Compiler *compiler, uint8_t i_arg)
{
	compiler_push_opcode(compiler, OpCode_LoadArg);
	compiler_push_u8(compiler, i_arg);
}

static void compiler_bytecode_store_local(Compiler *compiler, uint8_t i_local)
{
	compiler_push_opcode(compiler, OpCode_StoreLocal);
	compiler_push_u8(compiler, i_local);
}

static void compiler_bytecode_load_local(Compiler *compiler, uint8_t i_local)
{
	compiler_push_opcode(compiler, OpCode_LoadLocal);
	compiler_push_u8(compiler, i_local);
}

static void compiler_bytecode_debug_label(Compiler *compiler, sv label)
{
	compiler_push_opcode(compiler, OpCode_DebugLabel);
	compiler_push_sv(compiler, label);
}


// compiler functions
static void compiler_lookup_function_str(Compiler *compiler, sv function_name, uint32_t *out_module, uint32_t *out_function)
{
	CompilerModule *current_module = &compiler->module;
	// Search local functions
	for (uint32_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		sv local_function_name = string_pool_get(&compiler->vm->identifiers_pool, function->name);
		if (sv_equals(local_function_name, function_name)) {
			*out_module = ~(uint32_t)(0);
			*out_function = i_function;
			return;
		}
	}
	// Search imports
	for (uint32_t i_import = 0; i_import < current_module->imports_length; ++i_import) {
		uint32_t i_imported_module = current_module->imports[i_import];
		CompilerModule *imported_module = compiler->vm->compiler_modules + i_imported_module;
		for (uint32_t i_function = 0; i_function < imported_module->functions_length; ++i_function) {
			Function *function = imported_module->functions + i_function;
			sv imported_function_name = string_pool_get(&compiler->vm->identifiers_pool, function->name);
			bool is_importable = function->type == FunctionType_Global || function->type == FunctionType_Foreign;
			if (is_importable && sv_equals(imported_function_name, function_name)) {
				*out_module = i_import;
				*out_function = i_function;
				return;
			}
		}
	}
	*out_module = ~0u;
	*out_function = ~0u;
}

static void compiler_lookup_function_id(Compiler *compiler, StringId function_id, uint32_t *out_module, uint32_t *out_function)
{
	CompilerModule *current_module = &compiler->module;
	// Search local functions
	for (uint32_t i_function = 0; i_function < current_module->functions_length; ++i_function) {
		Function *function = current_module->functions + i_function;
		if (function->name.id == function_id.id) {
			*out_module = ~(uint32_t)(0);
			*out_function = i_function;
			return;
		}
	}
	// Search imports
	for (uint32_t i_import = 0; i_import < current_module->imports_length; ++i_import) {
		uint32_t i_imported_module = current_module->imports[i_import];
		CompilerModule *imported_module = compiler->vm->compiler_modules + i_imported_module;
		for (uint32_t i_function = 0; i_function < imported_module->functions_length; ++i_function) {
			Function *function = imported_module->functions + i_function;
			bool is_importable = function->type == FunctionType_Global || function->type == FunctionType_Foreign;
			if (is_importable && function->name.id == function_id.id) {
				*out_module = i_import;
				*out_function = i_function;
				return;
			}
		}
	}
	*out_module = ~0u;
	*out_function = ~0u;
}

uint32_t type_get_size(CompilerModule *module, TypeID id)
{
	if (id.builtin.is_user_defined != 0) {
		if (id.user_defined.index >= module->types_length) {
			return ~0u;
		}
		return module->types[id.user_defined.index].size;
	} else if (id.builtin.kind == BuiltinTypeKind_Signed || id.builtin.kind == BuiltinTypeKind_Unsigned) {
		return 1 << id.builtin.number_width;
	} else {
		const uint32_t BuiltinTypeKind_size[] = {
			0,
			0,
			0,
			1,
			4,
			8,
			8,
		};
		_Static_assert(ARRAY_LENGTH(BuiltinTypeKind_size) == BuiltinTypeKind_Count, "fail");
		return BuiltinTypeKind_size[(uint32_t)id.builtin.kind];
	}
}

static void compiler_push_scope(Compiler *compiler)
{
	if (compiler->scopes_length >= ARRAY_LENGTH(compiler->scopes)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return;
	}

	LexicalScope *new_scope = compiler->scopes + compiler->scopes_length;
	compiler->scopes_length += 1;
	*new_scope = (LexicalScope){0};
}

static void compiler_pop_scope(Compiler *compiler)
{
	if (compiler->scopes_length == 0) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return;
	}
	
	compiler->scopes_length -= 1;
}

static void compiler_push_loop_end_ip(Compiler *compiler, uint32_t loop_end_ip)
{
	if (compiler->loop_end_ips_length >= ARRAY_LENGTH(compiler->loop_end_ips)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return;
	}

	uint32_t *new_loop_end_ip = compiler->loop_end_ips + compiler->loop_end_ips_length;
	compiler->loop_end_ips_length += 1;
	*new_loop_end_ip = loop_end_ip;
}

static void compiler_pop_loop_end_ip(Compiler *compiler)
{
	if (compiler->loop_end_ips_length == 0 || compiler->loop_end_ips_length >= ARRAY_LENGTH(compiler->loop_end_ips)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return;
	}

	compiler->loop_end_ips_length -= 1;
}

static uint32_t compiler_last_loop_end_ip(Compiler *compiler)
{
	if (compiler->loop_end_ips_length == 0 || compiler->loop_end_ips_length >= ARRAY_LENGTH(compiler->loop_end_ips)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return 0;
	}

	return compiler->loop_end_ips[compiler->loop_end_ips_length-1];
}

static bool compiler_push_variable(Compiler *compiler, Token identifier_token, TypeID type, uint32_t *i_variable_out)
{
	if (compiler->scopes_length >= ARRAY_LENGTH(compiler->scopes)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return false;
	}

	uint32_t last_scope_index = compiler->scopes_length - 1;
	LexicalScope *current_scope = compiler->scopes + last_scope_index;
	if (current_scope->variables_length >= SCOPE_MAX_VARIABLES) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return false;
	}

	uint32_t i_new_variable = current_scope->variables_length;
	current_scope->variables_length += 1;

	sv name_str = string_pool_get(&compiler->compunit->string_pool, identifier_token.data.sid);

	current_scope->variables_name[i_new_variable] = name_str;
	current_scope->variables_type[i_new_variable] = type;

	*i_variable_out = i_new_variable;
	return true;
}

static bool compiler_push_arg(Compiler *compiler, Token identifier_token, TypeID type, uint32_t *i_arg_out)
{
	if (compiler->scopes_length >= ARRAY_LENGTH(compiler->scopes)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return false;
	}

	uint32_t last_scope_index = compiler->scopes_length - 1;
	LexicalScope *current_scope = compiler->scopes + last_scope_index;
	if (current_scope->variables_length >= SCOPE_MAX_VARIABLES) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return false;
	}

	uint32_t i_new_arg = current_scope->args_length;
	current_scope->args_length += 1;

	sv name_str = string_pool_get(&compiler->compunit->string_pool, identifier_token.data.sid);

	current_scope->args_name[i_new_arg] = name_str;
	current_scope->args_type[i_new_arg] = type;

	*i_arg_out = i_new_arg;
	return true;
}

static bool compiler_lookup_variable(Compiler *compiler, Token identifier_token, TypeID *type_out, uint32_t *i_variable_out, bool *is_variable)
{
	if (compiler->scopes_length == 0) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return false;
	}

	sv tofind_name = string_pool_get(&compiler->compunit->string_pool, identifier_token.data.sid);

	for (uint32_t i_scope = compiler->scopes_length - 1; i_scope < compiler->scopes_length; --i_scope) {
		LexicalScope *scope = compiler->scopes + i_scope;

		for (uint32_t i_variable = 0; i_variable < scope->variables_length; ++i_variable) {
			sv variable_name = scope->variables_name[i_variable];
			if (sv_equals(tofind_name, variable_name)) {
				*type_out = scope->variables_type[i_variable];
				*i_variable_out = i_variable;
				*is_variable = true;
				return true;
			}
		}

		for (uint32_t i_arg = 0; i_arg < scope->args_length; ++i_arg) {
			sv arg_name = scope->args_name[i_arg];
			if (sv_equals(tofind_name, arg_name)) {
				*type_out = scope->args_type[i_arg];
				*i_variable_out = i_arg;
				*is_variable = false;
				return true;
			}
		}
	}

	return false;
}

// compiler

static TypeID compile_sexpr(Compiler *compiler, const AstNode *node);

//

static TypeID compile_atom(Compiler *compiler, Token token)
{
	if (token.kind == TokenKind_Identifier) {
		// Refer a declared variable
		// <identifier>
		TypeID ty = UNIT_TYPE;
		uint32_t i_variable = 0;
		bool is_variable = true;
		if (!compiler_lookup_variable(compiler, token, &ty, &i_variable, &is_variable)) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
			compiler->compunit->error.span = token.span;
			return UNIT_TYPE;
		}
		if (is_variable) {
			compiler_bytecode_load_local(compiler, (uint8_t)(i_variable));
		} else {
			compiler_bytecode_load_arg(compiler, (uint8_t)(i_variable));
		}
		return ty;
	} else if (token.kind == TokenKind_UnsignedNumber) {
		// An integer constant
		// <number>
		uint32_t token_number = token.data.u32;
		compiler_bytecode_push_u32(compiler, token_number);
		// Try to reduce the number size as much as possible.
		TypeID type_id = type_id_new_builtin(BuiltinTypeKind_Unsigned);
		if (token_number <= 0xFFu) {
			type_id.builtin.number_width = NumberWidth_8;
		} else if (token_number <= 0xFFFFu) {
			// u16
			type_id.builtin.number_width = NumberWidth_16;
		} else if (token_number <= 0xFFFFFFFFu) {
			// u32
			type_id.builtin.number_width = NumberWidth_32;
		}
		else {
			// u64
		}
		return type_id;
	} else if (token.kind == TokenKind_SignedNumber) {
		// An integer constant
		// (-)<number>
		int32_t token_number = token.data.i32;
		compiler_bytecode_push_i32(compiler, token_number);
		// Try to reduce the number size as much as possible.
		TypeID type_id = type_id_new_builtin(BuiltinTypeKind_Signed);
		if (-0x7F-1 <= token_number && token_number <= 0x7F) {
			type_id.builtin.number_width = NumberWidth_8;
		} else if (-0x7FFF-1 <= token_number && token_number <= 0x7FFF) {
			// i16
			type_id.builtin.number_width = NumberWidth_16;
		} else if (-0x7FFFFFFF-1 <= token_number && token_number <= 0x7FFFFFFF) {
			// i32
			type_id.builtin.number_width = NumberWidth_32;
		}
		else {
			// i64
		}
		return type_id;
	} else if (token.kind == TokenKind_FloatingNumber) {
		// An floating point constant
		// <number>.<number>
		float token_number = token.data.f32;
		compiler_bytecode_push_f32(compiler, token_number);
		TypeID type_id = type_id_new_builtin(BuiltinTypeKind_Float);
		return type_id;
	} else if (token.kind == TokenKind_StringLiteral) {
		// A string literal
		// "str"
		sv literal = string_pool_get(&compiler->compunit->string_pool, token.data.sid);
		compiler_bytecode_push_str(compiler, literal);
		TypeID new_type_id = type_id_pointer_from(type_id_new_unsigned(NumberWidth_8));
		new_type_id.slice.builtin_kind = BuiltinTypeKind_Slice;
		return new_type_id;
	} else {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
}

// <identifier> | <number> | <s-expression>
static TypeID compile_expr(Compiler *compiler, const AstNode *node)
{
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (ast_is_atom(node)) {
		const Token token = ast_get_token(compiler->compunit->tokens, node);
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

static TypeID compile_sexprs_return_last(Compiler *compiler, const AstNode *node)
{
	const AstNode *first_expr_node = node;
	TypeID return_type = compile_expr(compiler, first_expr_node);

	uint32_t i_next_expr_node = first_expr_node->right_sibling_index;
	while (ast_is_valid(i_next_expr_node)) {
		const AstNode *next_expr_node = ast_get_node(compiler->compunit->nodes, i_next_expr_node);
		return_type = compile_expr(compiler, next_expr_node);
		i_next_expr_node = next_expr_node->right_sibling_index;
	}

	return return_type;
}

static TypeID compile_function_defininition(Compiler *compiler, const AstNode *node, FunctionType function_type)
{
	// Parse the function definition
	DefineNode define_node = {0};
	parse_define_sig(compiler->compunit, node, &define_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	parse_define_body(compiler->compunit, node, &define_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	sv function_name = string_pool_get(&compiler->compunit->string_pool, define_node.function_name_token.data.sid);
	StringId function_id = string_pool_intern(&compiler->vm->identifiers_pool, function_name);
	TypeID return_type = parse_type(compiler->compunit, &compiler->module, define_node.return_type_node);
	TypeID arg_types[MAX_ARGUMENTS] = {0};
	for (uint32_t i_arg = 0; i_arg < define_node.args_length; ++i_arg) {
		arg_types[i_arg] = parse_type(compiler->compunit, &compiler->module, define_node.arg_nodes[i_arg]);
	}
	// Look for duplicated symbols
	uint32_t i_found_module = 0;
	uint32_t i_found_function = 0;
	compiler_lookup_function_id(compiler, function_id, &i_found_module, &i_found_function);
	if (i_found_module < compiler->vm->compiler_modules_length || i_found_module < compiler->module.functions_length) {
		// Actually it should be possible to recompile a function if signature has not changed.
		INIT_ERROR(&compiler->compunit->error, ErrorCode_DuplicateSymbol);
		return UNIT_TYPE;
	}
	// Create a new function symbol in the module
	CompilerModule *current_module = &compiler->module;
	if (current_module->functions_length + 1 > ARRAY_LENGTH(current_module->functions)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
	Function *function = current_module->functions + current_module->functions_length;
	function->name = function_id;
	function->address = current_module->bytecode_length;
	function->type = function_type;
	function->return_type = return_type;
	current_module->functions_length += 1;
	// Compile the function code
	// Add a debug label to identify functions easily in the bytecode
	compiler_bytecode_debug_label(compiler, function_name);
	// Create a variable scope
	compiler_push_scope(compiler);
	for (uint32_t i_arg = 0; i_arg < define_node.args_length; ++i_arg) {
		TypeID arg_type = arg_types[i_arg];
		function->arg_types[function->arg_count] = arg_types[i_arg];
		function->arg_count += 1;
		uint32_t i_variable = 0;
		if (!compiler_push_arg(compiler, define_node.arg_identifiers[i_arg], arg_type, &i_variable)) {
			return UNIT_TYPE;
		}
	}
	TypeID body_type = compile_sexprs_return_last(compiler, define_node.body_node);
	// TODO: As of now, the return opcode always pop a value from the stack. So we have to make sure
	// that the stack is not empty when the function does not return anything!
	// Nothing is preventing functions from returning multiple values, so ideally functions should have a return value count? (or implement tuples?)
	if (type_similar(body_type, UNIT_TYPE)) {
		compiler_bytecode_push_u32(compiler, 0u);
	}
	compiler_pop_scope(compiler);
	compiler_push_opcode(compiler, OpCode_Ret);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	// Typecheck the body expression
	bool valid_return_type = type_similar(body_type, return_type);
	if (!valid_return_type) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.expected_type = return_type;
		compiler->compunit->error.got_type = body_type;
		compiler->compunit->error.span = define_node.body_node->span;
	}
	return function->return_type;
}

// Defines a new local function
// (define (<name> <return_type>) (<args>*) <expression>+)
static TypeID compile_define(Compiler *compiler, const AstNode *node)
{
	return compile_function_defininition(compiler, node, FunctionType_Local);
}

// Defines a new global function
// (define-global (<name> <return_type>) (<args>*) <expression>+)
static TypeID compile_define_global(Compiler *compiler, const AstNode *node)
{
	return compile_function_defininition(compiler, node, FunctionType_Global);
}

// Defines a new foreign function
// (define-foreign (<name> <return_type>) (<args>))
static TypeID compile_define_foreign(Compiler *compiler, const AstNode *node)
{
	DefineNode nodes = {0};
	parse_define_sig(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	sv function_name_token_str = string_pool_get(&compiler->compunit->string_pool, nodes.function_name_token.data.sid);
	StringId function_id = string_pool_intern(&compiler->vm->identifiers_pool, function_name_token_str);\

	// -- Type checking
	CompilerModule *current_module = &compiler->module;
	uint32_t i_found_module = 0;
	uint32_t i_found_function = 0;
	compiler_lookup_function_id(compiler, function_id, &i_found_module, &i_found_function);
	if (i_found_module < compiler->vm->compiler_modules_length || i_found_module < compiler->module.functions_length) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_DuplicateSymbol);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	if (current_module->functions_length + 1 >= ARRAY_LENGTH(current_module->functions)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	// Add to foreign function list
	uint32_t foreign_functions_length = current_module->foreign_functions_length;
	if (foreign_functions_length + 1 >= ARRAY_LENGTH(current_module->foreign_functions_name)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	current_module->foreign_functions_module_name[foreign_functions_length] = current_module->name;
	current_module->foreign_functions_name[foreign_functions_length] = function_id;
	current_module->foreign_functions_length += 1;

	// Create a new function symbol
	Function *function = current_module->functions + current_module->functions_length;
	function->name = function_id;
	function->address = foreign_functions_length;
	function->type = FunctionType_Foreign;
	// parse return type
	TypeID return_type = parse_type(compiler->compunit, &compiler->module, nodes.return_type_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	function->return_type = return_type;
	// parse argument types
	for (uint32_t i_arg = 0; i_arg < nodes.args_length; ++i_arg) {
		TypeID arg_type = parse_type(compiler->compunit, &compiler->module, nodes.arg_nodes[i_arg]);
		function->arg_types[function->arg_count] = arg_type;
		function->arg_count += 1;
	}
	current_module->functions_length += 1;
	
	return return_type;
}

// Defines a new struct
static TypeID compile_struct(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	StructNode nodes = {0};
	parse_struct(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	sv struct_name_token_str = string_pool_get(&compiler->compunit->string_pool, nodes.struct_name_token.data.sid);

	// -- Type checking
	// Check if the type is already defined
	for (uint32_t i_type = 0; i_type < compiler->module.types_length; ++i_type) {
		UserDefinedType *type = compiler->module.types + i_type;
		if (sv_equals(type->name, struct_name_token_str)) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode_DuplicateSymbol);
			compiler->compunit->error.span = node->span;
			// Actually it should be possible to recompile a function if signature has not changed.
			return UNIT_TYPE;
		}
	}
	// Not enough space to add a type
	if (compiler->module.types_length + 1 >= ARRAY_LENGTH(compiler->module.types)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	// -- Create a new structure type
	if (nodes.fields_length >= MAX_STRUCT_FIELDS) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}
	TypeID fields_type[MAX_STRUCT_FIELDS] = {0};

	TypeID struct_type_id = type_id_new_user_defined(compiler->module.types_length);
	compiler->module.types_length += 1;

	UserDefinedType *struct_type = compiler->module.types + struct_type_id.user_defined.index;
	*struct_type = (UserDefinedType){0};
	struct_type->size = 0;
	struct_type->name = struct_name_token_str;

	uint32_t struct_size = 0;
	for (uint32_t i_field = 0; i_field < nodes.fields_length; ++i_field) {
		TypeID field_type = parse_type(compiler->compunit, &compiler->module, nodes.field_type_nodes[i_field]);
		if (compiler->compunit->error.code != ErrorCode_Ok) {
			return UNIT_TYPE;
		}

		struct_type->field_types[i_field] = field_type;
		struct_type->field_names[i_field] = string_pool_get(&compiler->compunit->string_pool, nodes.field_identifiers[i_field].data.sid);
		struct_type->field_offsets[i_field] = struct_size;

		struct_size += type_get_size(&compiler->module, field_type);
		fields_type[i_field] = field_type;
	}

	struct_type->field_count = nodes.fields_length;
	struct_type->size = struct_size;

	return struct_type_id;
}

static TypeID compile_require(Compiler *compiler, const AstNode *node)
{
	(void)compiler;
	(void)node;
	return UNIT_TYPE;
}

// Conditional branch
static TypeID compile_if(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	IfNode nodes = {0};
	parse_if(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// Compile the condition first,
	TypeID cond_expr = compile_expr(compiler, nodes.cond_expr_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	
	TypeID bool_type = type_id_new_builtin(BuiltinTypeKind_Bool);
	if (!type_similar(cond_expr, bool_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = nodes.cond_expr_node->span;
		compiler->compunit->error.expected_type = type_id_new_builtin(BuiltinTypeKind_Bool);
		compiler->compunit->error.got_type = cond_expr;
		return UNIT_TYPE;
	}

	// If true, jump to the true branch (patch the jump adress later)
	uint32_t *jump_to_true_branch = compiler_bytecode_conditional_jump(compiler, 0);

	// Then compile the else branch, because the condition was false
	TypeID else_expr = compile_expr(compiler, nodes.else_expr_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// Jump over the true branch (patch the jump adress later)
	uint32_t *jump_to_end = compiler_bytecode_jump(compiler, 0);

	// Compile the true branch
	const uint32_t then_branch_address = compiler->module.bytecode_length;
	TypeID then_expr = compile_expr(compiler, nodes.then_expr_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	const uint32_t end_address = compiler->module.bytecode_length;
	*jump_to_true_branch = then_branch_address;
	*jump_to_end = end_address;

	bool valid_return_type = type_similar(else_expr, then_expr);
	if (!valid_return_type) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = node->span;
		compiler->compunit->error.expected_type = then_expr;
		compiler->compunit->error.got_type = else_expr;
	}

	return then_expr;
}

static TypeID compile_when(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *when_node = ast_get_left_child(compiler->compunit->nodes, node);
	// A when expression must have at least a condition
	if (!ast_has_right_sibling(when_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedExpr);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}
	const AstNode *cond_node = ast_get_right_sibling(compiler->compunit->nodes, when_node);
	// A when expression must have at least one expression after the condition
	if (!ast_has_right_sibling(cond_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedExpr);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}
	const AstNode *expr_node = ast_get_right_sibling(compiler->compunit->nodes, cond_node);
	
	// Compile the condition first,
	TypeID cond_expr = compile_expr(compiler, cond_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	TypeID bool_type = type_id_new_builtin(BuiltinTypeKind_Bool);
	if (!type_similar(cond_expr, bool_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = cond_node->span;
		compiler->compunit->error.expected_type = type_id_new_builtin(BuiltinTypeKind_Bool);
		compiler->compunit->error.got_type = cond_expr;
		return UNIT_TYPE;
	}

	// If true, jump to the true branch (patch the jump adress later)
	uint32_t *jump_to_true_branch = compiler_bytecode_conditional_jump(compiler, 0);

	// "False" branch: jump over the true branch (patch the jump adress later)
	uint32_t *jump_to_end = compiler_bytecode_jump(compiler, 0);

	// Compile the true branch
	const uint32_t then_branch_address = compiler->module.bytecode_length;
	TypeID then_expr = compile_sexprs_return_last(compiler, expr_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	const uint32_t end_address = compiler->module.bytecode_length;
	*jump_to_true_branch = then_branch_address;
	*jump_to_end = end_address;

	bool expr_returns_value = !type_similar(UNIT_TYPE, then_expr);
	if (expr_returns_value) {
		// Pop the stack if the expr returns a value.
		compiler_push_opcode(compiler, OpCode_Pop);
	}

	return UNIT_TYPE;
}

static TypeID compile_unless(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *when_node = ast_get_left_child(compiler->compunit->nodes, node);
	// A when expression must have at least a condition
	if (!ast_has_right_sibling(when_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedExpr);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}
	const AstNode *cond_node = ast_get_right_sibling(compiler->compunit->nodes, when_node);
	// A when expression must have at least one expression after the condition
	if (!ast_has_right_sibling(cond_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedExpr);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}
	const AstNode *expr_node = ast_get_right_sibling(compiler->compunit->nodes, cond_node);
	
	// Compile the condition first,
	TypeID cond_expr = compile_expr(compiler, cond_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	TypeID bool_type = type_id_new_builtin(BuiltinTypeKind_Bool);
	if (!type_similar(cond_expr, bool_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = cond_node->span;
		compiler->compunit->error.expected_type = type_id_new_builtin(BuiltinTypeKind_Bool);
		compiler->compunit->error.got_type = cond_expr;
		return UNIT_TYPE;
	}

	// If true, jump to the over the branch (path the jump address later)
	uint32_t *jump_to_end = compiler_bytecode_conditional_jump(compiler, 0);

	// "False" branch: Execute the body (patch the jump address later)
	uint32_t *jump_to_body = compiler_bytecode_jump(compiler, 0);

	// Compile the true branch
	const uint32_t body_address = compiler->module.bytecode_length;
	TypeID body_expr = compile_sexprs_return_last(compiler, expr_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	const uint32_t end_address = compiler->module.bytecode_length;
	*jump_to_end = end_address;
	*jump_to_body = body_address;

	bool expr_returns_value = !type_similar(UNIT_TYPE, body_expr);
	if (expr_returns_value) {
		// Pop the stack if the expr returns a value.
		compiler_push_opcode(compiler, OpCode_Pop);
	}

	return UNIT_TYPE;
}

static TypeID compile_loop(Compiler *compiler, const AstNode *node)
{
	const AstNode *loop_node = ast_get_left_child(compiler->compunit->nodes, node);
	// A loop expression must have at least one expr
	if (!ast_has_right_sibling(loop_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedExpr);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	uint32_t *jump_to_loop_begin = compiler_bytecode_jump(compiler, 0);
	uint32_t jump_to_loop_end_ip = compiler->module.bytecode_length;
	uint32_t *jump_to_loop_end = compiler_bytecode_jump(compiler, 0);
	// Push the address of the jump to the loop end, break will jump there.
	compiler_push_loop_end_ip(compiler, jump_to_loop_end_ip);

	// Get the beginning of the loop
	uint32_t loop_begin_ip = compiler->module.bytecode_length;
	compiler_push_opcode(compiler, OpCode_Nop);
	// Compile exprs
	const AstNode *first_sexpr = ast_get_right_sibling(compiler->compunit->nodes, loop_node);
	/*TypeID last_expr_type =*/ compile_sexprs_return_last(compiler, first_sexpr);
	// Jump to the beginning of the loop
	compiler_bytecode_jump(compiler, loop_begin_ip);
	uint32_t loop_end_ip = compiler->module.bytecode_length;

	compiler_pop_loop_end_ip(compiler);

	// Fixup jump addresses
	*jump_to_loop_begin = loop_begin_ip;
	*jump_to_loop_end = loop_end_ip;
	
	return UNIT_TYPE;
}

static TypeID compile_break(Compiler *compiler, const AstNode *node)
{
	const AstNode *break_node = ast_get_left_child(compiler->compunit->nodes, node);
	// A break expression DOES NOT have any children
	if (ast_has_right_sibling(break_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_UnexpectedExpression);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	uint32_t jump_to_end_of_the_loop_ip = compiler_last_loop_end_ip(compiler);
	compiler_bytecode_jump(compiler, jump_to_end_of_the_loop_ip);
	return UNIT_TYPE;
}

static TypeID compile_let(Compiler *compiler, const AstNode *node)
{
	LetNode nodes = {0};
	parse_let(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	compiler_bytecode_push_u32(compiler, 0u); // reserve space on the stack for the local
	
	TypeID expr_type = compile_expr(compiler, nodes.value_node);
	uint32_t i_variable = 0;
	if (!compiler_push_variable(compiler, nodes.name_token, expr_type, &i_variable)) {
		return UNIT_TYPE;
	}

	compiler_bytecode_store_local(compiler, (uint8_t)(i_variable));
	return UNIT_TYPE;
}

static TypeID compile_set(Compiler *compiler, const AstNode *node)
{
	LetNode nodes = {0};
	parse_let(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID variable_type = UNIT_TYPE;
	uint32_t i_variable = 0;
	bool is_variable = true;
	if (!compiler_lookup_variable(compiler, nodes.name_token, &variable_type, &i_variable, &is_variable)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
		compiler->compunit->error.span = nodes.name_token.span;
		return UNIT_TYPE;
	}

	TypeID expr_type = compile_expr(compiler, nodes.value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (!type_similar(expr_type, variable_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.expected_type = variable_type;
		compiler->compunit->error.got_type = expr_type;
		compiler->compunit->error.span = nodes.value_node->span;
		return UNIT_TYPE;
	}

	if (is_variable) {
		compiler_bytecode_store_local(compiler, (uint8_t)i_variable);
	}
	else {
		compiler_bytecode_store_arg(compiler, (uint8_t)i_variable);
	}
	return UNIT_TYPE;
}

// (begin <expr1> <expr2> ...)
static TypeID compile_begin(Compiler *compiler, const AstNode *node)
{
	const AstNode *begin_node = ast_get_left_child(compiler->compunit->nodes, node);
	// A begin expression must have at least one expr
	if (!ast_has_right_sibling(begin_node)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedExpr);
		compiler->compunit->error.span = node->span;
		return UNIT_TYPE;
	}

	const AstNode *first_sexpr = ast_get_right_sibling(compiler->compunit->nodes, begin_node);
	return compile_sexprs_return_last(compiler, first_sexpr);
}


typedef struct CompileNumBinaryOpResult
{
	TypeID inferred_type;
	bool success;
} CompileNumBinaryOpResult;

// Parse 2 expressions, typecheck them according to their numeric type, and let the caller push the correct opcode
// (<op> <lhs> <rhs>)
static CompileNumBinaryOpResult compile_num_binary_op(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return (CompileNumBinaryOpResult){0};
	}

	// -- Type checking
	TypeID lhs = compile_expr(compiler, nodes[0]);
	TypeID rhs = compile_expr(compiler, nodes[1]);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return (CompileNumBinaryOpResult){0};
	}

	// Both types need to be numeric
	bool lhs_numeric = type_id_is_builtin(lhs) && (lhs.builtin.kind == BuiltinTypeKind_Signed || lhs.builtin.kind == BuiltinTypeKind_Unsigned || lhs.builtin.kind == BuiltinTypeKind_Float);
	if (!lhs_numeric) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedNumericType);
		compiler->compunit->error.expected_type = UNIT_TYPE;
		compiler->compunit->error.got_type = lhs;
		compiler->compunit->error.span = nodes[0]->span;
		return (CompileNumBinaryOpResult){0};
	}
	bool rhs_numeric = type_id_is_builtin(rhs) && (rhs.builtin.kind == BuiltinTypeKind_Signed || rhs.builtin.kind == BuiltinTypeKind_Unsigned || lhs.builtin.kind == BuiltinTypeKind_Float);
	if (!rhs_numeric) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedNumericType);
		compiler->compunit->error.expected_type = UNIT_TYPE;
		compiler->compunit->error.got_type = rhs;
		compiler->compunit->error.span = nodes[1]->span;
		return (CompileNumBinaryOpResult){0};
	}

	TypeID expected_type = lhs;
	if (lhs.builtin.kind != BuiltinTypeKind_Float) {
		// We assume that the type of the operation:
		// - is the wider type
		// - is signed if one of the two operands is signed
		expected_type = type_id_new_unsigned(lhs.builtin.number_width);
		if (lhs.builtin.kind == BuiltinTypeKind_Signed || rhs.builtin.kind == BuiltinTypeKind_Signed) {
			expected_type.builtin.kind = BuiltinTypeKind_Signed;
		}
		if (rhs.builtin.number_width > lhs.builtin.number_width) {
			expected_type.builtin.number_width = rhs.builtin.number_width;
		}
	}	

	// Check that the two operands are similar to the expected type
	bool lhs_valid = type_similar(lhs, expected_type);
	if (!lhs_valid) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.expected_type = expected_type;
		compiler->compunit->error.got_type = lhs;
		compiler->compunit->error.span = nodes[0]->span;
		return (CompileNumBinaryOpResult){0};
	}	
	bool rhs_valid = type_similar(rhs, expected_type);
	if (!rhs_valid) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.expected_type = expected_type;
		compiler->compunit->error.got_type = rhs;
		compiler->compunit->error.span = nodes[1]->span;
		return (CompileNumBinaryOpResult){0};
	}
	
	CompileNumBinaryOpResult result = {0};
	result.inferred_type = expected_type;
	result.success = true;
	return result;
}

static TypeID compile_iadd(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}

	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_AddF32);
	} else if (res.inferred_type.builtin.number_width <= NumberWidth_32) {
		_Static_assert(OpCode_AddU8 + NumberWidth_32 == OpCode_AddU32);
		compiler_push_opcode(compiler, OpCode_AddU8 + res.inferred_type.builtin.number_width);
	} else {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;

	}
	return res.inferred_type;
}

static TypeID compile_isub(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}

	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_SubF32);
	} else if (res.inferred_type.builtin.number_width <= NumberWidth_32) {
		_Static_assert(OpCode_SubU8 + NumberWidth_32 == OpCode_SubU32);
		compiler_push_opcode(compiler, OpCode_SubU8 + res.inferred_type.builtin.number_width);
	} else {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
	return res.inferred_type;
}

static TypeID compile_imul(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}
	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_MulF32);
	} else {
		compiler_push_opcode(compiler, OpCode_MulI32);
	}
	return res.inferred_type;
}

// Compare two integers
// (<= <lhs> <rhs>)
static TypeID compile_ltethan(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}
	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_LteF32);
	} else  {
		compiler_push_opcode(compiler, OpCode_LteI32);
	}
	return type_id_new_builtin(BuiltinTypeKind_Bool);
}

// Compare two integers
// (< <lhs> <rhs>)
static TypeID compile_lthan(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}
	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_LtF32);
	} else {
		compiler_push_opcode(compiler, OpCode_LtI32);
	}
	return type_id_new_builtin(BuiltinTypeKind_Bool);
}

// Compare two integers
// (>= <lhs> <rhs>)
static TypeID compile_gtethan(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}
	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_GteF32);
	} else {	 
		compiler_push_opcode(compiler, OpCode_GteI32);
	}
	return type_id_new_builtin(BuiltinTypeKind_Bool);
}

// Compare two integers
// (> <lhs> <rhs>)
static TypeID compile_gthan(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}
	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_GtF32);
	} else{
		compiler_push_opcode(compiler, OpCode_GtI32);
	}
	return type_id_new_builtin(BuiltinTypeKind_Bool);
}

// Compare two integers
// (= <lhs> <rhs>)
static TypeID compile_eq(Compiler *compiler, const AstNode *node)
{
	CompileNumBinaryOpResult res = compile_num_binary_op(compiler, node);
	if (!res.success) {
		return UNIT_TYPE;
	}
	if (res.inferred_type.builtin.kind == BuiltinTypeKind_Float) {
		compiler_push_opcode(compiler, OpCode_EqF32);
	} else {
		compiler_push_opcode(compiler, OpCode_EqI32);
	}
	return type_id_new_builtin(BuiltinTypeKind_Bool);
}

// Perform a logical AND
// (and <lhs> <rhs>)
static TypeID compile_and(Compiler *compiler, const AstNode *node)
{
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID lhs = compile_expr(compiler, nodes[0]);
	TypeID rhs = compile_expr(compiler, nodes[1]);
	TypeID expected_type = type_id_new_builtin(BuiltinTypeKind_Bool);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (!type_similar(lhs, expected_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		__debugbreak();
		compiler->compunit->error.expected_type = expected_type;
		compiler->compunit->error.got_type = lhs;
		compiler->compunit->error.span = nodes[0]->span;
		return UNIT_TYPE;
	}
	if (!type_similar(rhs, expected_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.expected_type = expected_type;
		compiler->compunit->error.got_type = rhs;
		compiler->compunit->error.span = nodes[1]->span;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, OpCode_And);
	return expected_type;
}
// Perform a logical OR
// (or <lhs> <rhs>)
static TypeID compile_or(Compiler *compiler, const AstNode *node)
{
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID lhs = compile_expr(compiler, nodes[0]);
	TypeID rhs = compile_expr(compiler, nodes[1]);
	TypeID expected_type = type_id_new_builtin(BuiltinTypeKind_Bool);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (!type_similar(lhs, expected_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		__debugbreak();
		compiler->compunit->error.expected_type = expected_type;
		compiler->compunit->error.got_type = lhs;
		compiler->compunit->error.span = nodes[0]->span;
		return UNIT_TYPE;
	}
	if (!type_similar(rhs, expected_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.expected_type = expected_type;
		compiler->compunit->error.got_type = rhs;
		compiler->compunit->error.span = nodes[1]->span;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, OpCode_Or);
	return expected_type;
}

static TypeID compiler_load_pointer(Compiler *compiler, TypeID pointed_type)
{
	uint32_t type_size = type_get_size(&compiler->module, pointed_type);
	OpCode opcode = OpCode_Halt;
	if (type_size == 1) {
		opcode = OpCode_Load8;
	} else if (type_size == 2) {
		opcode = OpCode_Load16;
	} else if (type_size == 4) {
		opcode = OpCode_Load32;
	} else if (type_size == 8) {
		opcode = OpCode_Load64;
	} else {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
	
	compiler_push_opcode(compiler, opcode);
	return pointed_type;
}

// Load a value at the given memory address
// (load <addr>)
static TypeID compile_load(Compiler *compiler, const AstNode *node)
{
	const AstNode *value_node = NULL;
	parse_nary_op(compiler->compunit, node, 1, &value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID addr_type_id = compile_expr(compiler, value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// Typecheck
	if (!type_id_is_pointer(addr_type_id)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = value_node->span;
		compiler->compunit->error.expected_type = (TypeID){0};
		compiler->compunit->error.got_type = addr_type_id;
		return UNIT_TYPE;
	}

	TypeID pointed_type_id = type_id_deref_pointer(addr_type_id);
	return compiler_load_pointer(compiler, pointed_type_id);
}

static TypeID compiler_store_pointer(Compiler *compiler, TypeID pointed_type)
{
	uint32_t value_type_size = type_get_size(&compiler->module, pointed_type);
	
	OpCode opcode = OpCode_Halt;
	if (value_type_size == 1) {
		opcode = OpCode_Store8;
	} else if (value_type_size == 2) {
		opcode = OpCode_Store16;
	} else if (value_type_size == 4) {
		opcode = OpCode_Store32;
	} else {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
	
	compiler_push_opcode(compiler, opcode);
	return UNIT_TYPE;
}

// Store a value at the given memory address
// (store <addr> <value>)
static TypeID compile_store(Compiler *compiler, const AstNode *node)
{
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID addr_type_id = compile_expr(compiler, nodes[0]);
	TypeID expr_type_id = compile_expr(compiler, nodes[1]);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// Typecheck
	TypeID expected_pointer_type = type_id_pointer_from(expr_type_id);
	if (!type_similar(addr_type_id, expected_pointer_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = nodes[0]->span;
		compiler->compunit->error.expected_type = expected_pointer_type;
		compiler->compunit->error.got_type = addr_type_id;
		return UNIT_TYPE;
	}

	return compiler_store_pointer(compiler, expr_type_id);
}

// Returns the size of a type
static TypeID compile_sizeof(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *value_node = NULL;
	parse_nary_op(compiler->compunit, node, 1, &value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID expr_type = parse_type(compiler->compunit, &compiler->module, value_node);
	uint32_t type_size = type_get_size(&compiler->module, expr_type);
	compiler_bytecode_push_u32(compiler, type_size);
	return type_id_new_unsigned(NumberWidth_32);
}

static TypeID compile_data(Compiler *compiler, const AstNode *node)
{
	const AstNode *value_node = NULL;
	parse_nary_op(compiler->compunit, node, 1, &value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	const TypeID expr_type = compile_expr(compiler, value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (!type_id_is_slice(expr_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = value_node->span;
		compiler->compunit->error.got_type = expr_type;
		compiler->compunit->error.expected_type = type_id_new_builtin(BuiltinTypeKind_Slice);
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, OpCode_SliceData);
	
	TypeID slice_as_pointer = expr_type;
	slice_as_pointer.builtin.kind = BuiltinTypeKind_Pointer;
	return slice_as_pointer;
}

static TypeID compile_len(Compiler *compiler, const AstNode *node)
{
	const AstNode *value_node = NULL;
	parse_nary_op(compiler->compunit, node, 1, &value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	const TypeID expr_type = compile_expr(compiler, value_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (!type_id_is_slice(expr_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = value_node->span;
		compiler->compunit->error.got_type = expr_type;
		compiler->compunit->error.expected_type = type_id_new_builtin(BuiltinTypeKind_Slice);
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, OpCode_SliceLength);
	
	return type_id_new_unsigned(NumberWidth_32);
}

// (field <expr> <identifier>)
static TypeID compile_field(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// -- Typecheck
	TypeID pointer_type = compile_expr(compiler, nodes[0]);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (!type_id_is_pointer(pointer_type)) {
		// The expr type has to be a pointer
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot)
		compiler->compunit->error.got_type = pointer_type;
		compiler->compunit->error.expected_type = type_id_pointer_from(UNIT_TYPE);
		compiler->compunit->error.span = nodes[0]->span;
		return UNIT_TYPE;
	}
	TypeID struct_type = type_id_deref_pointer(pointer_type);
	if (!type_id_is_user_defined(struct_type)) {
		// The pointed type HAS to be a struct.
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = nodes[0]->span;
		compiler->compunit->error.got_type = pointer_type;
		compiler->compunit->error.expected_type = UNIT_TYPE;
		return UNIT_TYPE;
	}

	Token field_token = array_check(compiler->compunit->tokens, nodes[1]->atom_token_index);
	const bool is_an_identifier = ast_is_atom(nodes[1]) && field_token.kind == TokenKind_Identifier;
	if (!is_an_identifier) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedIdentifier);
		compiler->compunit->error.span = nodes[1]->span;
		return UNIT_TYPE;
	}
	sv field_identifier_str = string_pool_get(&compiler->compunit->string_pool, field_token.data.sid);
	// -- Find field offset
	if (struct_type.user_defined.index >= compiler->module.types_length) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
	UserDefinedType *type = compiler->module.types + struct_type.user_defined.index;
	uint32_t i_field = 0;
	for (; i_field < type->field_count; ++i_field) {
		if (sv_equals(type->field_names[i_field], field_identifier_str)) {
			break;
		}
	}
	if (i_field >= type->field_count) {
		// The field was not found in the struct.
		INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
		compiler->compunit->error.span = field_token.span;
		return UNIT_TYPE;
	}

	uint32_t field_offset = type->field_offsets[i_field];
	compiler_bytecode_push_u32(compiler, field_offset);
	compiler_push_opcode(compiler, OpCode_AddU32);

	TypeID field_type = type->field_types[i_field];
	TypeID pointer_to_field_type = type_id_pointer_from(field_type);
	return pointer_to_field_type;
}

// (store-field <expr> <identifier> <value>)
static TypeID compile_store_field(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *nodes[3] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// -- Typecheck
	TypeID pointer_type = compile_expr(compiler, nodes[0]);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	if (!type_id_is_pointer(pointer_type)) {
		// The expr type has to be a pointer
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot)
		compiler->compunit->error.got_type = pointer_type;
		compiler->compunit->error.expected_type = type_id_pointer_from(UNIT_TYPE);
		compiler->compunit->error.span = nodes[0]->span;
		return UNIT_TYPE;
	}
	TypeID struct_type = type_id_deref_pointer(pointer_type);
	if (!type_id_is_user_defined(struct_type)) {
		// The pointed type HAS to be a struct.
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = nodes[0]->span;
		compiler->compunit->error.got_type = pointer_type;
		compiler->compunit->error.expected_type = UNIT_TYPE;
		return UNIT_TYPE;
	}
	// Generate the field offset
	uint32_t *field_offset_to_fix = compiler_bytecode_push_u32(compiler, 0);
	compiler_push_opcode(compiler, OpCode_AddU32);

	Token field_token = array_check(compiler->compunit->tokens, nodes[1]->atom_token_index);
	const bool is_an_identifier = ast_is_atom(nodes[1]) && field_token.kind == TokenKind_Identifier;
	if (!is_an_identifier) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedIdentifier);
		compiler->compunit->error.span = nodes[1]->span;
		return UNIT_TYPE;
	}
	sv field_identifier_str = string_pool_get(&compiler->compunit->string_pool, field_token.data.sid);
	// -- Find field offset
	if (struct_type.user_defined.index >= compiler->module.types_length) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
	UserDefinedType *type = compiler->module.types + struct_type.user_defined.index;
	uint32_t i_field = 0;
	for (; i_field < type->field_count; ++i_field) {
		if (sv_equals(type->field_names[i_field], field_identifier_str)) {
			break;
		}
	}
	if (i_field >= type->field_count) {
		// The field was not found in the struct.
		INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
		compiler->compunit->error.span = field_token.span;
		return UNIT_TYPE;
	}

	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID expr_type = compile_expr(compiler, nodes[2]);
	TypeID field_type = type->field_types[i_field];
	if (!type_similar(expr_type, field_type)) {
		// The expression type has to match the field type
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot)
		compiler->compunit->error.got_type = expr_type;
		compiler->compunit->error.expected_type = field_type;
		compiler->compunit->error.span = nodes[2]->span;
		return UNIT_TYPE;
	}

	*field_offset_to_fix = type->field_offsets[i_field];

	return compiler_store_pointer(compiler, type->field_types[i_field]);
}

// (load-field <expr> <identifier>)
static TypeID compile_load_field(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// -- Typecheck
	TypeID expr_type = compile_expr(compiler, nodes[0]);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	if (!type_id_is_pointer(expr_type)) {
		// The expr type has to be a pointer
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot)
		compiler->compunit->error.got_type = expr_type;
		compiler->compunit->error.expected_type = type_id_pointer_from(UNIT_TYPE);
		compiler->compunit->error.span = nodes[0]->span;
		return UNIT_TYPE;
	}
	TypeID struct_type = type_id_deref_pointer(expr_type);
	if (!type_id_is_user_defined(struct_type)) {
		// The pointed type HAS to be a struct.
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = nodes[0]->span;
		compiler->compunit->error.got_type = expr_type;
		compiler->compunit->error.expected_type = UNIT_TYPE;
		return UNIT_TYPE;
	}
	// Generate the pointer offset
	uint32_t *field_offset_to_fix = compiler_bytecode_push_u32(compiler, 0);
	compiler_push_opcode(compiler, OpCode_AddU32);			

	Token field_token = array_check(compiler->compunit->tokens, nodes[1]->atom_token_index);
	const bool is_an_identifier = ast_is_atom(nodes[1]) && field_token.kind == TokenKind_Identifier;
	if (!is_an_identifier) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedIdentifier);
		compiler->compunit->error.span = nodes[1]->span;
		return UNIT_TYPE;
	}
	sv field_identifier_str = string_pool_get(&compiler->compunit->string_pool, field_token.data.sid);

	// -- Find field offset
	if (struct_type.user_defined.index >= compiler->module.types_length) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}
	UserDefinedType *type = compiler->module.types + struct_type.user_defined.index;
	for (uint32_t i_field = 0; i_field < type->field_count; ++i_field) {
		if (sv_equals(type->field_names[i_field], field_identifier_str)) {
			*field_offset_to_fix = type->field_offsets[i_field];
			TypeID result = compiler_load_pointer(compiler, type->field_types[i_field]);
			return result;
		}
	}

	// The field was not found in the struct.
	INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
	compiler->compunit->error.span = field_token.span;
	return UNIT_TYPE;
}
 
static TypeID compile_field_offset(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	// -- Typecheck
	TypeID expr_type = parse_type(compiler->compunit, &compiler->module, nodes[0]);
	if (!type_id_is_user_defined(expr_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = nodes[0]->span;
		compiler->compunit->error.got_type = expr_type;
		return UNIT_TYPE;
	}

	bool is_an_identifier = ast_is_atom(nodes[1]);
	Token field_token = array_check(compiler->compunit->tokens, nodes[1]->atom_token_index);
	is_an_identifier = is_an_identifier && field_token.kind == TokenKind_Identifier;
	if (!is_an_identifier) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedIdentifier);
		compiler->compunit->error.span = nodes[1]->span;
		return UNIT_TYPE;
	}
	sv field_identifier_str = string_pool_get(&compiler->compunit->string_pool, field_token.data.sid);

	// -- Find field offset
	if (expr_type.user_defined.index >= compiler->module.types_length) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_Fatal);
		return UNIT_TYPE;
	}

	UserDefinedType *type = compiler->module.types + expr_type.user_defined.index;
	for (uint32_t i_field = 0; i_field < type->field_count; ++i_field) {
		if (sv_equals(type->field_names[i_field], field_identifier_str)) {
			uint32_t val = type->field_offsets[i_field];
			compiler_bytecode_push_u32(compiler, val);
			return type_id_new_unsigned(NumberWidth_32);
		}
	}

	INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
	compiler->compunit->error.span = field_token.span;
	return UNIT_TYPE;
}

// Allocate memory on the stack (_stack_alloc <type> <size>)
static TypeID compile_stack_alloc(Compiler *compiler, const AstNode *node)
{
	// -- Parsing
	const AstNode *nodes[2] = {0};
	parse_nary_op(compiler->compunit, node, ARRAY_LENGTH(nodes), nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID type_of_pointed_memory = parse_type(compiler->compunit, &compiler->module, nodes[0]);

	/*TypeID size_type =*/compile_expr(compiler, nodes[1]);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}


	// TODO: We need to bound check the alloc, <size> > sizeof(<type>
	// TODO: Implement
	compiler_bytecode_push_u32(compiler, 42u);
	return type_id_pointer_from(type_of_pointed_memory);
}

static TypeID compile_ptr_offset(Compiler *compiler, const AstNode *node)
{
	PtrOffsetNodes nodes = {0};
	parse_ptr_offset(compiler->compunit, node, &nodes);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}

	TypeID return_type = parse_type(compiler->compunit, &compiler->module, nodes.return_type_node);
	if (!type_id_is_pointer(return_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot)
		compiler->compunit->error.got_type = return_type;
		compiler->compunit->error.span = nodes.return_type_node->span;
		return UNIT_TYPE;
	}

	TypeID base_pointer_type = compile_expr(compiler, nodes.base_pointer_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	if (!type_id_is_pointer(base_pointer_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot)
		compiler->compunit->error.got_type = return_type;
		compiler->compunit->error.span = nodes.base_pointer_node->span;
		return UNIT_TYPE;
	}

	// Check that the provided offset is an int
	TypeID offset_type = compile_expr(compiler, nodes.offset_node);
	if (compiler->compunit->error.code != ErrorCode_Ok) {
		return UNIT_TYPE;
	}
	TypeID expected_offset_type = type_id_new_unsigned(NumberWidth_32);
	if (!type_similar(offset_type, expected_offset_type)) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
		compiler->compunit->error.span = nodes.offset_node->span;
		compiler->compunit->error.got_type = offset_type;
		compiler->compunit->error.expected_type = expected_offset_type;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, OpCode_AddU32);
	return return_type;
}

typedef TypeID (*CompilerBuiltin)(Compiler *, const AstNode *);

// There are two kinds of "builtins", the ones allowed at top-level and the other ones
const CompilerBuiltin compiler_top_builtins[] = {
	compile_define,
	compile_define_global,
	compile_define_foreign,
	compile_struct,
	compile_require,
};
const sv compiler_top_builtins_str[] = {
	SV_LIT("define"),
	SV_LIT("define-global"),
	SV_LIT("define-foreign"),
	SV_LIT("struct"),
	SV_LIT("require"),
};
_Static_assert(ARRAY_LENGTH(compiler_top_builtins_str) == ARRAY_LENGTH(compiler_top_builtins));

const CompilerBuiltin compiler_expr_builtins[] = {
	compile_if,
	compile_when,
	compile_unless,
	compile_loop,
	compile_break,
	compile_let,
	compile_set,
	compile_begin,
	compile_iadd,
	compile_isub,
	compile_imul,
	compile_ltethan,
	compile_lthan,
	compile_gtethan,
	compile_gthan,
	compile_eq,
	compile_and,
	compile_or,
	compile_store,
	compile_load,
	compile_sizeof,
	compile_data,
	compile_len,
	compile_field,
	compile_store_field,
	compile_load_field,
	compile_field_offset,
	compile_stack_alloc,
	compile_ptr_offset,
};
const sv compiler_expr_builtins_str[] = {
	SV_LIT("if"),
	SV_LIT("when"),
	SV_LIT("unless"),
	SV_LIT("loop"),
	SV_LIT("break"),
	SV_LIT("let"),
	SV_LIT("set"),
	SV_LIT("begin"),
	SV_LIT("+"),
	SV_LIT("-"),
	SV_LIT("*"),
	SV_LIT("<="),
	SV_LIT("<"),
	SV_LIT(">="),
	SV_LIT(">"),
	SV_LIT("="),
	SV_LIT("and"),
	SV_LIT("or"),
	SV_LIT("_store"),
	SV_LIT("_load"),
	SV_LIT("_sizeof"),
	SV_LIT("_data"),
	SV_LIT("_len"),
	SV_LIT("_field"),
	SV_LIT("_store_field"),
	SV_LIT("_load_field"),
	SV_LIT("_field_offset"),
	SV_LIT("_stack_alloc"),
	SV_LIT("_ptr_offset"),
};
_Static_assert(ARRAY_LENGTH(compiler_expr_builtins_str) == ARRAY_LENGTH(compiler_expr_builtins));

// (<identifier> <expr>*)
static TypeID compile_sexpr(Compiler *compiler, const AstNode *function_node)
{
	// Get the function name
	const AstNode *identifier_node = ast_get_left_child(compiler->compunit->nodes, function_node);
	const Token identifier = ast_get_token(compiler->compunit->tokens, identifier_node);
	sv identifier_str = string_pool_get(&compiler->compunit->string_pool, identifier.data.sid);

	// Dispatch to the correct builtin compile function (define, iadd, etc)
	const uint32_t compiler_builtin_length = ARRAY_LENGTH(compiler_expr_builtins);
	for (uint32_t i = 0; i < compiler_builtin_length; ++i) {
		if (sv_equals(identifier_str, compiler_expr_builtins_str[i])) {
			return compiler_expr_builtins[i](compiler, function_node);
		}
	}

	// If the identifier is not a builtin, generate a function call
	uint32_t i_found_module = 0;
	uint32_t i_found_function = 0;
	compiler_lookup_function_str(compiler, identifier_str, &i_found_module, &i_found_function);
	bool is_external = i_found_module < compiler->vm->compiler_modules_length;
	if (!is_external && i_found_function >= compiler->module.functions_length) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
		compiler->compunit->error.span = identifier.span;
		return UNIT_TYPE;
	}
	Function *found_function = NULL;
	if (is_external) {
		found_function = compiler->vm->compiler_modules[i_found_module].functions + i_found_function;
	} else {
		found_function = compiler->module.functions + i_found_function;
	}

	// Typecheck arguments
	uint32_t i_sig_arg_type = 0;
	uint32_t i_arg_node = identifier_node->right_sibling_index;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_node = ast_get_node(compiler->compunit->nodes, i_arg_node);
		// There is an expr but the signature has ended
		if (i_sig_arg_type >= found_function->arg_count) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode_UnexpectedExpression);
			compiler->compunit->error.span = arg_node->span;
			return UNIT_TYPE;
		}

		// compile expr
		TypeID arg_type = compile_expr(compiler, arg_node);
		if (compiler->compunit->error.code != ErrorCode_Ok) {
			return UNIT_TYPE;
		}

		// typecheck
		if (!type_similar(arg_type, found_function->arg_types[i_sig_arg_type])) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedTypeGot);
			compiler->compunit->error.span = arg_node->span;
			compiler->compunit->error.expected_type = found_function->arg_types[i_sig_arg_type];
			compiler->compunit->error.got_type = arg_type;
		}

		i_arg_node = arg_node->right_sibling_index;
		i_sig_arg_type += 1;
	}

	// There is an argument left in the signature
	if (i_sig_arg_type < found_function->arg_count) {
		INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedExpr);
		compiler->compunit->error.span = function_node->span;
		compiler->compunit->error.expected_type = found_function->arg_types[i_sig_arg_type];
		compiler->compunit->error.got_type = UNIT_TYPE;
		return UNIT_TYPE;
	}

	// -- The found function signature matched
	if (found_function->type == FunctionType_Foreign) {
		uint32_t i_foreign_function = 0;
		if (is_external) {
			CompilerModule *external_module = compiler->vm->compiler_modules + i_found_module;
			
			// If the foreign function is imported from another module, we need to check
			// if we have already imported it.
			CompilerModule *current_module = &compiler->module;
			uint32_t f = 0;
			for (; f < current_module->foreign_functions_length; ++f)
			{
				if (current_module->foreign_functions_name[f].id == found_function->name.id)
				{
					break;
				}
			}
			// If we haven't found it, add it to our own import
			if (f >= current_module->foreign_functions_length)
			{
				if (current_module->foreign_functions_length >= ARRAY_LENGTH(current_module->foreign_functions_name)) {
					__debugbreak();
				}
				f = current_module->foreign_functions_length;
				current_module->foreign_functions_module_name[f] = external_module->name;
				current_module->foreign_functions_name[f] = found_function->name;
				current_module->foreign_functions_length += 1;
			}
			i_foreign_function = f;
		}
		else {
			i_foreign_function = found_function->address;
		}
		compiler_bytecode_call_foreign(compiler, (uint8_t)i_foreign_function, (uint8_t)(found_function->arg_count));
	}
	else {
		if (is_external) {
			// Insert the external function into our import list
			CompilerModule *current_module = &compiler->module;
			uint32_t i_imported_function = 0;
			for (; i_imported_function < current_module->imported_functions_length; ++i_imported_function)
			{
				if (current_module->imported_module_indices[i_imported_function] == i_found_module
				    && current_module->imported_function_indices[i_imported_function] == i_found_function)
				{
					break;
				}
			}
			if (i_imported_function >= current_module->imported_functions_length)
			{
				uint32_t new_i = current_module->imported_functions_length;
				current_module->imported_module_indices[new_i] = i_found_module;
				current_module->imported_function_indices[new_i] = i_found_function;
				current_module->imported_functions_length += 1;
			}
			compiler_bytecode_call_external(compiler, (uint8_t)(i_imported_function), (uint8_t)(found_function->arg_count));
		} else {
			compiler_bytecode_call(compiler, found_function->address, (uint8_t)(found_function->arg_count));
		}
	}

	// As of now, functions that return () still push 0 on the stack. This way the executor always pop when returning from a function.
	if (type_similar(found_function->return_type, UNIT_TYPE)) {
		compiler_push_opcode(compiler, OpCode_Pop);
	}

	return found_function->return_type;
}

void compile_module(Compiler *compiler)
{
	const AstNode *root_node = compiler->compunit->nodes;

	// Set instruction #0 to halt to detect invalid jump to 0
	compiler_push_opcode(compiler, OpCode_Halt);

	// Compile every S-expression at root level
	uint32_t i_root_expr = root_node->left_child_index;
	while (ast_is_valid(i_root_expr)) {
		const AstNode *root_expr = ast_get_node(compiler->compunit->nodes, i_root_expr);

		// Validate that a root S-expression starts with a token
		const AstNode *first_sexpr_member = ast_get_left_child(compiler->compunit->nodes, root_expr);
		const bool is_an_atom = ast_has_left_child(root_expr) && ast_is_atom(first_sexpr_member);
		if (!is_an_atom) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedIdentifier);
			return;
		}
		Token atom_token = array_check(compiler->compunit->tokens, first_sexpr_member->atom_token_index);
		const bool is_an_identifier = atom_token.kind == TokenKind_Identifier;
		if (!is_an_identifier) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode_ExpectedIdentifier);
			return;
		}

		// Find the compiler builtin for this S-expression
		sv identifier_str = string_pool_get(&compiler->compunit->string_pool, atom_token.data.sid);
		uint32_t i_builtin = 0;
		for (; i_builtin < ARRAY_LENGTH(compiler_top_builtins); ++i_builtin) {
			if (sv_equals(identifier_str, compiler_top_builtins_str[i_builtin])) {
				compiler_top_builtins[i_builtin](compiler, root_expr);
				break;
			}
		}
		if (i_builtin >= ARRAY_LENGTH(compiler_top_builtins)) {
			INIT_ERROR(&compiler->compunit->error, ErrorCode_UnknownSymbol);
			compiler->compunit->error.span = first_sexpr_member->span;
		}
		if (compiler->compunit->error.code != ErrorCode_Ok) {
			break;
		}

		// Go to the next root expression
		i_root_expr = root_expr->right_sibling_index;
	}
}

ScanDepsResult compiler_scan_dependencies(Arena *out_memory, CompilationUnit *compunit)
{
	StringId REQUIRE_ID = string_pool_intern(&compunit->string_pool, SV("require"));

	ScanDepsResult result = {0};
	const AstNode *root_node = compunit->nodes;

	uint32_t i_root_expr = root_node->left_child_index;
	while (ast_is_valid(i_root_expr)) {
		const AstNode *root_expr = ast_get_node(compunit->nodes, i_root_expr);

		// Validate that a root S-expression starts with an identifier
		const AstNode *first_sexpr_member = ast_get_left_child(compunit->nodes, root_expr);
		bool is_an_atom = ast_has_left_child(root_expr) && ast_is_atom(first_sexpr_member);
		if (!is_an_atom) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			return result;
		}
		Token atom_token = array_check(compunit->tokens, first_sexpr_member->atom_token_index);
		const bool is_an_identifier = atom_token.kind == TokenKind_Identifier;
		if (!is_an_identifier) {
			INIT_ERROR(&compunit->error, ErrorCode_ExpectedIdentifier);
			return result;
		}

		// Find a require clause
		if (atom_token.data.sid.id == REQUIRE_ID.id) {
			// Check that there is only one argument
			if (root_expr->child_count < 2) {
				INIT_ERROR(&compunit->error, ErrorCode_ExpectedExpr);
				compunit->error.span = root_expr->span;
				return result;
			} else if (root_expr->child_count > 2) {
				INIT_ERROR(&compunit->error, ErrorCode_UnexpectedExpression);
				compunit->error.span = root_expr->span;
				return result;
			}

			const AstNode *require_path_node = ast_get_right_sibling(compunit->nodes, first_sexpr_member);

			// Check that the argument is a string
			is_an_atom = ast_is_atom(require_path_node);
			if (!is_an_atom) {
				INIT_ERROR(&compunit->error, ErrorCode_ExpectedString);
				compunit->error.span = require_path_node->span;
				return result;
			}
			Token require_path_token = array_check(compunit->tokens, require_path_node->atom_token_index);
			if (require_path_token.kind != TokenKind_StringLiteral) {
				INIT_ERROR(&compunit->error, ErrorCode_ExpectedString);
				compunit->error.span = require_path_node->span;
				return result;
			}

			// Process the require path
			StringId dependency_id = require_path_token.data.sid;
			// IMPORTANT: we are only allocating this id in the arena, so they will all be contigous.
			StringId *output_dependency_id = arena_alloc(out_memory, sizeof(StringId));
			if (result.names == NULL) {
				result.names = output_dependency_id;
			}
			*output_dependency_id = dependency_id;
			result.count += 1;
		}

		// Go to the next root expression
		i_root_expr = root_expr->right_sibling_index;
	}

	result.success = true;
	return result;
}

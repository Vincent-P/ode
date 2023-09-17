#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// TODO:
// Clean 16/09/23: Split all compile functions into "parse_xxx"/"compile_xxx"

#define SET_RESULT(x)                                                                                                  \
	__debugbreak();                                                                                                    \
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
	// error handling
	Result result;
	const char *result_file;
	int result_file_line;
	span error;
	Token got_token;
	TypeID expected_type;
	TypeID got_type;
};

// compiler helpers
void compiler_push_opcode(Compiler *, CompilerState *compstate, OpCodeKind opcode_kind)
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

uint32_t *compiler_push_u32(Compiler *, CompilerState *compstate, uint32_t value)
{
	Module *current_module = compstate->current_module;
	uint32_t to_write = sizeof(uint32_t);
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
	uint32_t to_write = sizeof(int32_t);
	if (current_module->bytecode_length + to_write >= current_module->bytecode_capacity) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return nullptr;
	}

	int32_t *bytecode_u32 = reinterpret_cast<int32_t *>(current_module->bytecode + current_module->bytecode_length);
	bytecode_u32[0] = value;
	current_module->bytecode_length += to_write;
	return bytecode_u32;
}

TypeID *compiler_push_type_id(Compiler *comp, CompilerState *compstate, TypeID id)
{
	return reinterpret_cast<TypeID *>(compiler_push_u32(comp, compstate, id.raw));
}

void compiler_push_sv(Compiler *, CompilerState *compstate, sv value)
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
	compiler_push_opcode(compiler, compstate, OpCodeKind::DebugLabel);
	compiler_push_sv(compiler, compstate, function_name);

	// Create a variable scope
	compiler_push_scope(compiler, compstate);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
		compiler_push_u32(compiler, compstate, uint32_t(args_length - 1 - i_arg));
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
	compiler_push_opcode(compiler, compstate, OpCodeKind::Ret);
	if (compstate->result != Result::Ok) {
		return nullptr;
	}

	bool valid_return_type = type_similar(compstate, return_type, body_type);
	if (!valid_return_type) {
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

void compiler_push_scope(Compiler *compiler, CompilerState *compstate)
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

	compiler_push_opcode(compiler, compstate, OpCodeKind::BeginScope);
}

void compiler_pop_scope(Compiler *compiler, CompilerState *compstate)
{
	if (compstate->scopes.length == 0) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::EndScope);

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

TypeID parse_type(Compiler *compiler, CompilerState *compstate, const AstNode *node)
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
			if (type_id_is_pointer(inner_type)) {
				inner_type.pointer.indirection_count += 1;
				return inner_type;
			} else if (type_id_is_builtin(inner_type)) {
				TypeID pointer_type = {};
				pointer_type.pointer = {};
				pointer_type.pointer.builtin_kind = inner_type.builtin.kind;
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
			} else {
				compstate->result = Result::Fatal;
				SET_RESULT(compstate->result);
				compstate->error = child1->span;
				return UNIT_TYPE;
			}
		}

		// We haven't found any builtin
		compstate->result = Result::CompilerUnknownSymbol;
		SET_RESULT(compstate->result);
		compstate->error = token->span;
		return UNIT_TYPE;
	}
}

TypeID compile_atom(Compiler *compiler, CompilerState *compstate, const AstNode *expr_node)
{
	const Token *token = ast_get_token(&compstate->tokens, expr_node);
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
		compiler_push_opcode(compiler, compstate, OpCodeKind::GetLocal);
		compiler_push_u32(compiler, compstate, uint32_t(i_variable));
		return ty;
	} else if (token->kind == TokenKind::Number) {
		// An integer constant
		// <number>
		int32_t token_number = sv_to_int(token_sv);
		TypeID new_type_id = type_id_new_builtin(BuiltinTypeKind::Int);
		compiler_push_opcode(compiler, compstate, OpCodeKind::Constant);
		compiler_push_i32(compiler, compstate, token_number);
		return new_type_id;
	} else if (token->kind == TokenKind::StringLiteral) {
		// A string literal
		// "str"
		sv value_sv = sv_substr(token_sv, span{1, uint32_t(token_sv.length) - 2});

		// TODO: It's slow.
		uint32_t i_constant = 0;
		uint32_t constants_length = compstate->current_module->constants_length;
		for (; i_constant < constants_length; ++i_constant) {
			sv constant_sv = compstate->current_module->constants[i_constant].str;
			if (sv_equals(value_sv, constant_sv)) {
				break;
			}
		}
		// The constant string was not found, add it.
		if (i_constant == constants_length) {
			if (constants_length >= compstate->current_module->constants_capacity) {

				compstate->result = Result::Fatal;
				SET_RESULT(compstate->result);
				return UNIT_TYPE;
			}
			compstate->current_module->constants_length += 1;
			compstate->current_module->constants[i_constant].str = value_sv;
		}

		compiler_push_opcode(compiler, compstate, OpCodeKind::ConstantStr);
		compiler_push_u32(compiler, compstate, i_constant);

		TypeID new_type_id = type_id_new_builtin(BuiltinTypeKind::Str);
		return new_type_id;
	} else {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		return UNIT_TYPE;
	}
}

// <identifier> | <number> | <s-expression>
TypeID compile_expr(Compiler *compiler, CompilerState *compstate, const AstNode *expr_node)
{
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	if (ast_is_atom(expr_node)) {
		return compile_atom(compiler, compstate, expr_node);
	} else if (ast_has_left_child(expr_node)) {
		return compile_sexpr(compiler, compstate, expr_node);
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
	Token arg_identifiers[MAX_ARGUMENTS] = {};
	const AstNode *arg_nodes[MAX_ARGUMENTS] = {};
	uint32_t args_length = 0;

	// -- Parsing
	const AstNode *define_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *name_type_node = ast_get_right_sibling(&compstate->nodes, define_token_node);

	// Get the function name node
	const AstNode *function_name_node = ast_get_left_child(&compstate->nodes, name_type_node);
	const bool function_name_is_an_atom = ast_has_left_child(name_type_node) && ast_is_atom(function_name_node);
	if (!function_name_is_an_atom) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const Token *function_name_token = ast_get_token(&compstate->tokens, function_name_node);
	// Get the function return type node
	if (!ast_has_right_sibling(function_name_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *return_type_node = ast_get_right_sibling(&compstate->nodes, function_name_node);

	// Get the arguments list node
	const AstNode *arglist_node = ast_get_right_sibling(&compstate->nodes, name_type_node);
	if (!ast_has_right_sibling(name_type_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	// Arguments are laid out as (name1 type1 name2 type2 ...)
	uint32_t i_arg_node = arglist_node->left_child_index;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_name_node = ast_get_node(&compstate->nodes, i_arg_node);
		const Token *arg_name = ast_get_token(&compstate->tokens, arg_name_node);
		const bool arg_is_an_identifier = ast_is_atom(arg_name_node) && arg_name->kind == TokenKind::Identifier;
		if (!arg_is_an_identifier) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return UNIT_TYPE;
		}

		const AstNode *arg_type_node = ast_get_right_sibling(&compstate->nodes, arg_name_node);
		if (!ast_has_right_sibling(arg_name_node)) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return UNIT_TYPE;
		}

		arg_identifiers[args_length] = *arg_name;
		arg_nodes[args_length] = arg_type_node;
		args_length += 1;

		i_arg_node = arg_type_node->right_sibling_index;
	}

	const AstNode *body_node = ast_get_right_sibling(&compstate->nodes, arglist_node);
	if (!ast_has_right_sibling(arglist_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	sv function_name_token_str = sv_substr(compstate->input.text, function_name_token->span);

	// -- Type checking
	TypeID return_type = parse_type(compiler, compstate, return_type_node);
	TypeID arg_types[MAX_ARGUMENTS] = {};
	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		arg_types[i_arg] = parse_type(compiler, compstate, arg_nodes[i_arg]);
	}

	Function *new_function = compiler_compile_function(compiler,
		compstate,
		function_name_token_str,
		return_type,
		arg_identifiers,
		arg_types,
		args_length,
		[&]() -> TypeID {
			// <-- Compile the body
			TypeID body_type = compile_sexprs_return_last(compiler, compstate, body_node);
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
	Token arg_identifiers[MAX_ARGUMENTS] = {};
	const AstNode *arg_nodes[MAX_ARGUMENTS] = {};
	uint32_t args_length = 0;

	// -- Parsing
	const AstNode *define_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *name_type_node = ast_get_right_sibling(&compstate->nodes, define_token_node);

	// Get the function name node
	const AstNode *function_name_node = ast_get_left_child(&compstate->nodes, name_type_node);
	const bool function_name_is_an_atom = ast_has_left_child(name_type_node) && ast_is_atom(function_name_node);
	if (!function_name_is_an_atom) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const Token *function_name_token = ast_get_token(&compstate->tokens, function_name_node);
	// Get the function return type node
	if (!ast_has_right_sibling(function_name_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *return_type_node = ast_get_right_sibling(&compstate->nodes, function_name_node);

	// Get the arguments list node
	const AstNode *arglist_node = ast_get_right_sibling(&compstate->nodes, name_type_node);
	if (!ast_has_right_sibling(name_type_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	// Arguments are laid out as (name1 type1 name2 type2 ...)
	uint32_t i_arg_node = arglist_node->left_child_index;
	while (ast_is_valid(i_arg_node)) {
		const AstNode *arg_name_node = ast_get_node(&compstate->nodes, i_arg_node);
		const Token *arg_name = ast_get_token(&compstate->tokens, arg_name_node);
		const bool arg_is_an_identifier = ast_is_atom(arg_name_node) && arg_name->kind == TokenKind::Identifier;
		if (!arg_is_an_identifier) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return UNIT_TYPE;
		}

		const AstNode *arg_type_node = ast_get_right_sibling(&compstate->nodes, arg_name_node);
		if (!ast_has_right_sibling(arg_name_node)) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = arglist_node->span;
			return UNIT_TYPE;
		}

		arg_identifiers[args_length] = *arg_name;
		arg_nodes[args_length] = arg_type_node;
		args_length += 1;

		i_arg_node = arg_type_node->right_sibling_index;
	}

	sv function_name_token_str = sv_substr(compstate->input.text, function_name_token->span);

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
	TypeID return_type = parse_type(compiler, compstate, return_type_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}
	function->return_type = return_type;

	// Add a debug label to identify functions easily in the bytecode
	compiler_push_opcode(compiler, compstate, OpCodeKind::DebugLabel);
	compiler_push_sv(compiler, compstate, function_name_token_str);

	// Create a variable scope
	compiler_push_scope(compiler, compstate);
	// All arguments are in the stack in opposite order.
	// Pop them into local slots
	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
		compiler_push_u32(compiler, compstate, uint32_t(args_length - 1 - i_arg));
	}

	for (uint32_t i_arg = 0; i_arg < args_length; ++i_arg) {
		TypeID arg_type = parse_type(compiler, compstate, arg_nodes[i_arg]);
		function->arg_types[function->arg_count] = arg_type;
		function->arg_count += 1;

		uint32_t i_variable = 0;
		if (!compiler_push_variable(compstate, &arg_identifiers[i_arg], arg_type, &i_variable)) {
			return UNIT_TYPE;
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
TypeID compile_struct(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
	const AstNode *struct_token_node = ast_get_left_child(&compstate->nodes, node);

	// Get struct name node
	const AstNode *struct_name_node = ast_get_right_sibling(&compstate->nodes, struct_token_node);
	if (!ast_has_right_sibling(struct_token_node) || !ast_is_atom(struct_name_node)) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const Token *struct_name_token = ast_get_token(&compstate->tokens, struct_name_node);
	// Get fields
	Token field_identifiers[MAX_STRUCT_FIELDS] = {};
	const AstNode *field_type_nodes[MAX_STRUCT_FIELDS] = {};
	uint32_t fields_length = 0;
	// Get the first field (a struct MUST have at least one field)
	if (!ast_has_right_sibling(struct_name_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	uint32_t i_field_node = struct_name_node->right_sibling_index;
	while (ast_is_valid(i_field_node)) {
		const AstNode *field_node = ast_get_node(&compstate->nodes, i_field_node);
		// Get the field name
		const AstNode *field_identifier_node = ast_get_left_child(&compstate->nodes, field_node);
		if (!ast_has_left_child(field_node)) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = field_node->span;
			return UNIT_TYPE;
		}
		const Token *field_identifier_token = ast_get_token(&compstate->tokens, field_identifier_node);
		const bool is_an_identifier_token_node =
			ast_is_atom(field_identifier_node) && field_identifier_token->kind == TokenKind::Identifier;
		if (!is_an_identifier_token_node) {
			compstate->result = Result::CompilerExpectedIdentifier;
			SET_RESULT(compstate->result);
			compstate->error = field_identifier_node->span;
			return UNIT_TYPE;
		}
		// Get the field type
		if (!ast_has_right_sibling(field_identifier_node)) {
			compstate->result = Result::CompilerExpectedExpr;
			SET_RESULT(compstate->result);
			compstate->error = field_node->span;
			return UNIT_TYPE;
		}
		const AstNode *field_type_node = ast_get_right_sibling(&compstate->nodes, field_identifier_node);
		if (fields_length > MAX_STRUCT_FIELDS) {
			compstate->result = Result::Fatal;
			SET_RESULT(compstate->result);
			compstate->error = field_identifier_node->span;
			return UNIT_TYPE;
		}

		field_identifiers[fields_length] = *field_identifier_token;
		field_type_nodes[fields_length] = field_type_node;
		fields_length += 1;

		i_field_node = field_node->right_sibling_index;
	}

	sv struct_name_token_str = sv_substr(compstate->input.text, struct_name_token->span);

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
	for (uint32_t i_field = 0; i_field < fields_length; ++i_field) {
		TypeID field_type = parse_type(compiler, compstate, field_type_nodes[i_field]);
		if (compstate->result != Result::Ok) {
			return UNIT_TYPE;
		}

		struct_type->field_types[i_field] = field_type;
		struct_type->field_names[i_field] = sv_substr(compstate->input.text, field_identifiers[i_field].span);
		struct_type->field_offsets[i_field] = struct_size;

		struct_size += type_get_size(compstate, field_type);
		fields_type[i_field] = field_type;
	}

	struct_type->field_count = fields_length;
	struct_type->size = struct_size;

	// -- Compile builtin-functions for the struct
	sv ctor_name = struct_name_token_str;
	/*Function *ctor =*/compiler_compile_function(compiler,
		compstate,
		ctor_name,
		struct_type_id,
		field_identifiers,
		fields_type,
		fields_length,
		[&]() -> TypeID {
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
			compiler_push_type_id(compiler, compstate, struct_type_id);
			compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
			compiler_push_u32(compiler, compstate, struct_local_index);

			for (uint32_t i_field = 0; i_field < fields_length; ++i_field) {
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

			return struct_type_id;
		});

	return struct_type_id;
}

// Conditional branch
// (if <cond_expression> <then_expression> <else_expression>)
TypeID compile_if(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	// -- Parsing
#define HANDLE_ERR                                                                                                     \
	compstate->result = Result::CompilerExpectedExpr;                                                                  \
	SET_RESULT(compstate->result);                                                                                     \
	compstate->error = node->span;                                                                                     \
	return UNIT_TYPE;

	const AstNode *if_token_node = ast_get_left_child(&compstate->nodes, node);
	if (!ast_has_right_sibling(if_token_node)) {
		// There must be a cond node
		HANDLE_ERR;
	}
	const AstNode *cond_expr_node = ast_get_right_sibling(&compstate->nodes, if_token_node);
	if (!ast_has_right_sibling(cond_expr_node)) {
		// There must be a then node
		HANDLE_ERR;
	}
	const AstNode *then_expr_node = ast_get_right_sibling(&compstate->nodes, cond_expr_node);
	if (!ast_has_right_sibling(then_expr_node)) {
		// There must be an else node
		HANDLE_ERR;
	}
	const AstNode *else_expr_node = ast_get_right_sibling(&compstate->nodes, then_expr_node);
	if (ast_has_right_sibling(else_expr_node)) {
		// There must not be a node after the else
		HANDLE_ERR;
	}
#undef HANDLE_ERR

	// Compile the condition first,
	/*TypeID cond_expr =*/compile_expr(compiler, compstate, cond_expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// If true, jump to the true branch (patch the jump adress later)
	compiler_push_opcode(compiler, compstate, OpCodeKind::ConditionalJump);
	uint32_t *jump_to_true_branch = compiler_push_u32(compiler, compstate, 0);

	// Then compile the else branch, because the condition was false
	TypeID else_expr = compile_expr(compiler, compstate, else_expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	// Jump over the true branch (patch the jump adress later)
	compiler_push_opcode(compiler, compstate, OpCodeKind::Jump);
	uint32_t *jump_to_end = compiler_push_u32(compiler, compstate, 0);

	// Compile the true branch
	const uint32_t then_branch_address = compstate->current_module->bytecode_length;
	TypeID then_expr = compile_expr(compiler, compstate, then_expr_node);
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
	// -- Parsing
	const AstNode *let_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *name_node = ast_get_right_sibling(&compstate->nodes, let_token_node);
	if (!ast_has_right_sibling(let_token_node) || !ast_is_atom(name_node)) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *value_node = ast_get_right_sibling(&compstate->nodes, name_node);
	if (!ast_has_right_sibling(name_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const Token *name_token = ast_get_token(&compstate->tokens, name_node);

	// -- Type checking
	// Compile the body
	TypeID expr_type = compile_expr(compiler, compstate, value_node);
	uint32_t i_variable = 0;
	if (!compiler_push_variable(compstate, name_token, expr_type, &i_variable)) {
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::SetLocal);
	compiler_push_u32(compiler, compstate, uint32_t(i_variable));

	return expr_type;
}

TypeID compile_begin(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *begin_node = ast_get_left_child(&compstate->nodes, node);
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
	const AstNode *op_token_node = ast_get_left_child(&compstate->nodes, node);
	const AstNode *lhs_node = ast_get_right_sibling(&compstate->nodes, op_token_node);
	if (!ast_has_right_sibling(op_token_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *rhs_node = ast_get_right_sibling(&compstate->nodes, lhs_node);
	if (!ast_has_right_sibling(lhs_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	if (ast_has_right_sibling(rhs_node)) {
		compstate->result = Result::CompilerTooManyArgs;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}

	// -- Type checking
	TypeID lhs = compile_expr(compiler, compstate, lhs_node);
	TypeID rhs = compile_expr(compiler, compstate, rhs_node);
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
		compstate->error = lhs_node->span;
		return UNIT_TYPE;
	}

	if (!rhs_valid) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = type;
		compstate->got_type = rhs;
		compstate->error = rhs_node->span;
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

// Returns a field of a struct
// (field <expr> <member identifier>)
TypeID compile_field(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *field_token_node = ast_get_left_child(&compstate->nodes, node);
	// Get the expression node
	const AstNode *expr_node = ast_get_right_sibling(&compstate->nodes, field_token_node);
	if (!ast_has_right_sibling(field_token_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	// Get the field identifier
	if (!ast_has_right_sibling(expr_node)) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *field_identifier_node = ast_get_right_sibling(&compstate->nodes, expr_node);
	const Token *field_identifier_token = ast_get_token(&compstate->tokens, field_identifier_node);
	if (!ast_is_atom(field_identifier_node) || field_identifier_token->kind != TokenKind::Identifier) {
		compstate->result = Result::CompilerExpectedIdentifier;
		SET_RESULT(compstate->result);
		compstate->error = field_identifier_node->span;
		return UNIT_TYPE;
	}

	// Typecheck
	TypeID expr_type_id = compile_expr(compiler, compstate, expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	if (!type_id_is_user_defined(expr_type_id)) {
		compstate->result = Result::CompilerExpectedStruct;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		compstate->got_type = expr_type_id;
		return UNIT_TYPE;
	}

	UserDefinedType *expr_type = compstate->current_module->types + expr_type_id.user_defined.index;
	const sv field_id_str = sv_substr(compstate->input.text, field_identifier_token->span);

	uint32_t field_count = expr_type->field_count;
	uint32_t i_found_field = ~0u;

	for (uint32_t i_field = 0; i_field < field_count; ++i_field) {
		if (sv_equals(expr_type->field_names[i_field], field_id_str)) {
			i_found_field = i_field;
			break;
		}
	}

	if (i_found_field == ~0u) {
		compstate->result = Result::CompilerUnknownField;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::GetField);
	compiler_push_u32(compiler, compstate, uint32_t(i_found_field));

	TypeID field_type = expr_type->field_types[i_found_field];
	return field_type;
}

// Returns the address of an object
// (addr <expr>)
TypeID compile_addr(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *addr_token_node = ast_get_left_child(&compstate->nodes, node);
	// Get the expression node
	if (!ast_has_right_sibling(addr_token_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *expr_node = ast_get_right_sibling(&compstate->nodes, addr_token_node);

	// Typecheck
	TypeID expr_type_id = compile_expr(compiler, compstate, expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::Addr);
	compiler_push_type_id(compiler, compstate, expr_type_id);

	TypeID pointer_to_expr_type_id = {};
	if (type_id_is_pointer(expr_type_id)) {
		pointer_to_expr_type_id = expr_type_id;
		pointer_to_expr_type_id.pointer.indirection_count += 1;
		return pointer_to_expr_type_id;
	} else if (type_id_is_builtin(expr_type_id)) {
		pointer_to_expr_type_id.pointer.builtin_kind = BuiltinTypeKind::Pointer;
		pointer_to_expr_type_id.pointer.pointee_builtin_kind = expr_type_id.builtin.kind;
		pointer_to_expr_type_id.pointer.indirection_count = 1;
		return pointer_to_expr_type_id;
	}
	// It has to be a user defined type if it's not a pointer or a builtin
	if (!type_id_is_user_defined(expr_type_id)) {
		compstate->result = Result::Fatal;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	pointer_to_expr_type_id.pointer.builtin_kind = BuiltinTypeKind::Pointer;
	pointer_to_expr_type_id.pointer.indirection_count = 1;
	pointer_to_expr_type_id.pointer.user_defined_index = expr_type_id.user_defined.index;
	return pointer_to_expr_type_id;
}

// Dereference a pointer
// (* <expr>)
TypeID compile_deref(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *addr_token_node = ast_get_left_child(&compstate->nodes, node);
	if (!ast_has_right_sibling(addr_token_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *expr_node = ast_get_right_sibling(&compstate->nodes, addr_token_node);

	// Typecheck
	TypeID expr_type_id = compile_expr(compiler, compstate, expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	if (!type_id_is_pointer(expr_type_id)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->expected_type = {};
		compstate->expected_type.pointer.builtin_kind = BuiltinTypeKind::Pointer;
		compstate->got_type = expr_type_id;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::Deref);
	compiler_push_type_id(compiler, compstate, expr_type_id);

	TypeID pointed_type_id = {};
	if (expr_type_id.pointer.pointee_builtin_kind != BuiltinTypeKind::Unit) {
		pointed_type_id = type_id_new_builtin(expr_type_id.pointer.pointee_builtin_kind);
	} else {
		// If the pointer is not pointing to a building, it has to be a valid user defined type
		if (expr_type_id.pointer.user_defined_index >= compstate->current_module->types_length) {
			compstate->result = Result::Fatal;
			SET_RESULT(compstate->result);
			return UNIT_TYPE;
		}

		pointed_type_id = type_id_new_user_defined(expr_type_id.pointer.user_defined_index);
	}
	return pointed_type_id;
}

// Write an int32_t at the given address
// (write-i32 <addr> <value>)
TypeID compile_write_i32(Compiler *compiler, CompilerState *compstate, const AstNode *node)
{
	const AstNode *write_token_node = ast_get_left_child(&compstate->nodes, node);
	// Get the addr node
	if (!ast_has_right_sibling(write_token_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *addr_node = ast_get_right_sibling(&compstate->nodes, write_token_node);
	// Get the value expr node
	if (!ast_has_right_sibling(addr_node)) {
		compstate->result = Result::CompilerExpectedExpr;
		SET_RESULT(compstate->result);
		compstate->error = node->span;
		return UNIT_TYPE;
	}
	const AstNode *expr_node = ast_get_right_sibling(&compstate->nodes, addr_node);

	// Typecheck
	TypeID addr_type_id = compile_expr(compiler, compstate, addr_node);
	TypeID expr_type_id = compile_expr(compiler, compstate, expr_node);
	if (compstate->result != Result::Ok) {
		return UNIT_TYPE;
	}

	TypeID int_pointer_type = {};
	int_pointer_type.pointer.builtin_kind = BuiltinTypeKind::Pointer;
	int_pointer_type.pointer.pointee_builtin_kind = BuiltinTypeKind::Int;
	int_pointer_type.pointer.indirection_count = 1;
	if (!type_similar(compstate, int_pointer_type, addr_type_id)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = addr_node->span;
		compstate->expected_type = int_pointer_type;
		compstate->got_type = addr_type_id;
		return UNIT_TYPE;
	}

	TypeID int_type = {};
	int_type.builtin.kind = BuiltinTypeKind::Int;
	if (!type_similar(compstate, int_type, expr_type_id)) {
		compstate->result = Result::CompilerExpectedTypeGot;
		SET_RESULT(compstate->result);
		compstate->error = expr_node->span;
		compstate->expected_type = int_type;
		compstate->got_type = expr_type_id;
		return UNIT_TYPE;
	}

	compiler_push_opcode(compiler, compstate, OpCodeKind::Addr);
	compiler_push_type_id(compiler, compstate, expr_type_id);
	return UNIT_TYPE;
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
	compile_field,
	compile_addr,
	compile_deref,
	compile_write_i32,
};
const sv compiler_expr_builtins_str[] = {
	sv_from_null_terminated("if"),
	sv_from_null_terminated("let"),
	sv_from_null_terminated("begin"),
	sv_from_null_terminated("+"),
	sv_from_null_terminated("-"),
	sv_from_null_terminated("<="),
	sv_from_null_terminated("field"),
	sv_from_null_terminated("addr"),
	sv_from_null_terminated("*"),
	sv_from_null_terminated("write-i32"),
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

		TypeID arg_type = compile_expr(compiler, compstate, arg_node);
		if (compstate->result != Result::Ok) {
			return UNIT_TYPE;
		}

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
	compiler_push_opcode(compiler, compstate, OpCodeKind::Call);
	compiler_push_u32(compiler, compstate, uint32_t(i_function));

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
	const uint32_t functions_capacity = 8;
	new_module->functions_capacity = functions_capacity;
	new_module->functions = static_cast<Function *>(calloc(functions_capacity, sizeof(Function)));

	const uint32_t bytecode_capacity = 1024;
	new_module->bytecode_capacity = bytecode_capacity;
	new_module->bytecode = static_cast<uint8_t *>(calloc(bytecode_capacity, sizeof(OpCodeKind)));

	const uint32_t types_capacity = 8;
	new_module->types_capacity = types_capacity;
	new_module->types = static_cast<UserDefinedType *>(calloc(types_capacity, sizeof(UserDefinedType)));

	const uint32_t constants_capacity = 16;
	new_module->constants_capacity = constants_capacity;
	new_module->constants = static_cast<Constant *>(calloc(constants_capacity, sizeof(Constant)));

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

		bool is_unary_u32 = opcode_kind == OpCodeKind::SetLocal || opcode_kind == OpCodeKind::GetLocal ||
		                    opcode_kind == OpCodeKind::GetField || opcode_kind == OpCodeKind::SetField ||
		                    opcode_kind == OpCodeKind::Call || opcode_kind == OpCodeKind::MakeStruct ||
		                    opcode_kind == OpCodeKind::ConstantStr;

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

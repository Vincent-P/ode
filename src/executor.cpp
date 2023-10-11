#include "executor.h"
#include "image.h"
#include <corecrt_wstdio.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct FunctionInstance
{
	ForeignFn foreign;
};

struct ModuleInstance
{
	FunctionInstance *functions;
	uint64_t functions_capacity;
	uint64_t functions_length;
};

struct ExecutorState
{
	ExecutorConfig config;
	Module *modules;
	uint64_t modules_length;
	uint64_t modules_capacity;
	ModuleInstance *module_instances;
	uint64_t module_instances_length;
	uint64_t module_instances_capacity;

	bool failed;
	sv error_msg;
	sv error_file;
	int error_line;
	uint64_t error_ip;
};

struct CallFrame
{
	uint64_t i_current_function;
	uint64_t return_ip;
};

#define VARSCOPE_CAPACITY 16
struct VariableScope
{
	StackValue variables[VARSCOPE_CAPACITY];
	uint64_t variables_length;
};

struct ExecutionContext
{
	Module *module;
	ModuleInstance *module_instance;

	CallFrame *callstack_begin;
	CallFrame *callstack_current;
	const CallFrame *callstack_end;

	VariableScope *scopes_begin;
	VariableScope *scopes_current;
	const VariableScope *scopes_end;

	StackValue *stack_begin;
	StackValue *stack_current;
	const StackValue *stack_end;

	uint8_t *locals_storage_begin;
	uint8_t *locals_storage_current;
	uint8_t *locals_storage_previous;
	uint8_t *locals_storage_end;

	uint64_t ip;
};

ExecutorState *executor_init(ExecutorConfig config)
{
	ExecutorState *state = reinterpret_cast<ExecutorState *>(calloc(1, sizeof(ExecutorState)));
	state->config = config;
	state->modules_capacity = 8;
	state->modules = static_cast<Module *>(calloc(state->modules_capacity, sizeof(Module)));
	state->module_instances_capacity = 8;
	state->module_instances =
		static_cast<ModuleInstance *>(calloc(state->module_instances_capacity, sizeof(ModuleInstance)));
	return state;
}

static void execution_init(ExecutionContext *context)
{
	context->callstack_begin = static_cast<CallFrame *>(calloc(128, sizeof(VariableScope)));
	context->callstack_current = context->callstack_begin - 1;
	context->callstack_end = context->callstack_begin + 128;
	context->scopes_begin = static_cast<VariableScope *>(calloc(128, sizeof(VariableScope)));
	context->scopes_current = context->scopes_begin - 1;
	context->scopes_end = context->scopes_begin + 128;
	context->stack_begin = static_cast<StackValue *>(calloc(128, sizeof(StackValue)));
	context->stack_current = context->stack_begin - 1;
	context->stack_end = context->stack_begin + 128;
	context->locals_storage_begin = static_cast<uint8_t *>(calloc(128, sizeof(uint8_t)));
	context->locals_storage_current = context->locals_storage_begin;
	context->locals_storage_previous = context->locals_storage_current;
	context->locals_storage_end = context->locals_storage_begin + 128;
}

void executor_assert_trigger(ExecutorState *state, uint64_t ip, sv condition_str, sv file, int line)
{
	// Don't hide errors
	if (state->failed == true) {
		return;
	}

	state->failed = true;
	state->error_msg = condition_str;
	state->error_file = file;
	state->error_line = line;
	state->error_ip = ip;

	__debugbreak();
}

#define EXEC_ASSERT(state, condition)                                                                                  \
	if ((condition) == false) {                                                                                        \
executor_assert_trigger(state, ctx->ip,                                                                                 \
			sv_from_null_terminated(#condition),                                                                       \
			sv_from_null_terminated(__FILE__),                                                                         \
			__LINE__);                                                                                                 \
	}

void executor_load_module(ExecutorState *state, Module *module)
{
	uint64_t i_module = 0;
	for (; i_module < state->modules_length; ++i_module) {
		if (sv_equals(state->modules[i_module].name, module->name)) {
			break;
		}
	}

	// No more space to add the module!
	if (i_module >= state->modules_capacity || i_module >= state->module_instances_capacity) {
		return;
	}

	// The module was not found. Add it.
	if (i_module == state->modules_length) {
		state->modules_length += 1;
		state->module_instances_length += 1;
	}

	// TODO: deep copy?
	state->modules[i_module] = *module;

	ModuleInstance *module_instance = state->module_instances + i_module;
	if (module_instance->functions_capacity == 0) {
		module_instance->functions_capacity = 8;
		module_instance->functions =
			static_cast<FunctionInstance *>(calloc(module_instance->functions_capacity, sizeof(FunctionInstance)));
	} else {
		memset(module_instance->functions, 0, sizeof(FunctionInstance) * module_instance->functions_capacity);
	}
	module_instance->functions_length = module->functions_length;

	for (uint64_t i_function = 0; i_function < module->functions_length; ++i_function) {
		if (module->functions[i_function].is_foreign) {
			const sv module_name = module->name;
			const sv function_name = module->functions[i_function].name;
			module_instance->functions[i_function].foreign = state->config.foreign_callback(module_name, function_name);
		}
	}
}

static void executor_execute_module_at(ExecutorState *state, uint64_t i_module, uint64_t first_ip);
void executor_execute_module_entrypoint(ExecutorState *state, sv module_name)
{
	const uint64_t modules_len = state->modules_length;
	uint64_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (sv_equals(state->modules[i_module].name, module_name)) {
			break;
		}
	}

	// Module not found
	if (i_module >= modules_len) {
		fprintf(stderr, "module \"%.*s\" not found\n", int(module_name.length), module_name.chars);
		return;
	}

	Module *module = state->modules + i_module;

	const uint64_t functions_len = module->functions_length;
	const sv entrypoint_name = sv_from_null_terminated("main");
	uint64_t i_entrypoint = 0;
	for (; i_entrypoint < functions_len; ++i_entrypoint) {
		if (sv_equals(module->functions[i_entrypoint].name, entrypoint_name)) {
			break;
		}
	}

	// Entrypoint not found
	if (i_entrypoint >= functions_len) {
		fprintf(stderr, "main not found\n");
		return;
	}

	const Function *entrypoint = module->functions + i_entrypoint;

	uint64_t ip = entrypoint->address;
	executor_execute_module_at(state, i_module, ip);
}

uint32_t type_get_size(ExecutorState *state, ExecutionContext *ctx, TypeID id)
{
	if (id.builtin.is_user_defined != 0) {
		EXEC_ASSERT(state, id.user_defined.index < ctx->module->types_length);
		if (state->failed) {
			return 0;
		}
		return ctx->module->types[id.user_defined.index].size;
	} else {
		return BuiltinTypeKind_size[uint32_t(id.builtin.kind)];
	}
}

static TypeID bytecode_read_type_id(ExecutorState *state, ExecutionContext *ctx)
{
	uint64_t bytecode_len = ctx->module->bytecode_length;
	uint8_t *bytecode = ctx->module->bytecode;

	if (ctx->ip + sizeof(TypeID) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read TypeID)\n");
		ctx->ip = bytecode_len;
		return type_id_new_builtin(BuiltinTypeKind::Unit);
	}
	TypeID *bytecode_type_id = reinterpret_cast<TypeID *>(bytecode + ctx->ip);
	ctx->ip = ctx->ip + sizeof(TypeID);
	return *bytecode_type_id;

	TypeID id = *bytecode_type_id;

	if (type_id_is_pointer(id)) {
		// TODO: Validate pointer types
	} else if (type_id_is_builtin(id)) {
		EXEC_ASSERT(state, id.builtin.kind < BuiltinTypeKind::Count);
	} else {
		EXEC_ASSERT(state, type_id_is_user_defined(id));
		EXEC_ASSERT(state, id.user_defined.index < ctx->module->types_length);
	}

	return id;
}

template <typename T>
static T bytecode_read_scalar(ExecutionContext *ctx)
{
	uint64_t bytecode_len = ctx->module->bytecode_length;
	uint8_t *bytecode = ctx->module->bytecode;

	if (ctx->ip + sizeof(T) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read u32)\n");
		ctx->ip = bytecode_len;
		return 0;
	}
	T *bytecode_as_t = reinterpret_cast<T *>(bytecode + ctx->ip);
	ctx->ip = ctx->ip + sizeof(T);
	return *bytecode_as_t;
}

static uint32_t bytecode_read_u32(ExecutionContext *ctx)
{
	return bytecode_read_scalar<uint32_t>(ctx);
}
static uint8_t bytecode_read_u8(ExecutionContext *ctx)
{
	return bytecode_read_scalar<uint8_t>(ctx);
}

static sv bytecode_read_sv(ExecutionContext *ctx)
{
	uint64_t bytecode_len = ctx->module->bytecode_length;
	uint8_t *bytecode = ctx->module->bytecode;
	uint64_t local_ip = ctx->ip;

	// Read string length
	if (local_ip + sizeof(uint32_t) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read sv length)\n");
		ctx->ip = bytecode_len;
		return {};
	}
	uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(bytecode + local_ip);
	sv result = {};
	result.length = *bytecode_u32;
	local_ip += sizeof(uint32_t);

	// Read string
	if (local_ip + result.length > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read sv chars)\n");
		ctx->ip = bytecode_len;
		return {};
	}
	char *bytecode_char = reinterpret_cast<char *>(bytecode + local_ip);
	result.chars = bytecode_char;

	ctx->ip = ctx->ip + sizeof(uint32_t) + result.length;

	return result;
}

static void execute_begin_scope(ExecutorState *state, ExecutionContext *ctx)
{
	EXEC_ASSERT(state, ctx->scopes_current < ctx->scopes_end);
	if (state->failed) {
		return;
	}
	ctx->scopes_current += 1;
	ctx->scopes_current->variables_length = 0;
	memset(ctx->scopes_current->variables, 0, sizeof(ctx->scopes_current->variables));
}

static void execute_end_scope(ExecutorState *state, ExecutionContext *ctx)
{
	EXEC_ASSERT(state, ctx->scopes_current >= ctx->scopes_begin);
	if (state->failed) {
		return;
	}
	ctx->scopes_current -= 1;
}

StackValue execution_get_local(ExecutionContext *ctx, uint32_t i_local)
{
	return ctx->scopes_current->variables[i_local];
}

sv execution_get_str(ExecutionContext *ctx, Str str)
{
	if (str.is_constant == 1) {
		if (str.offset < ctx->module->constant_strings_length) {
			return ctx->module->constant_strings[str.offset];
		}
	}
	return sv_from_null_terminated("<ERROR>");
}

static void print_ip(ExecutionContext *ctx)
{
	printf("%zu\t| ", ctx->ip);
}

static void print_indent(long long n)
{
	for (int32_t i = 0; i < (n+1); ++i) {
		putchar(' ');
		putchar(' ');
		putchar(' ');
		putchar(' ');
	}
}

static void log_push(ExecutorState *, ExecutionContext *ctx, StackValue value)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("push %s %d\n", StackValueKind_str[uint8_t(value.kind)], value.i32);
}

static void log_pop(ExecutorState *, ExecutionContext *ctx, StackValue value)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("pop  %s %d\n", StackValueKind_str[uint8_t(value.kind)], value.i32);
}

static StackValue *push_operand(ExecutorState *state, ExecutionContext *ctx, StackValue value_to_push)
{
	EXEC_ASSERT(state, ctx->stack_current + 1 < ctx->stack_end);
	if (state->failed) {
		return nullptr;
	}

	ctx->stack_current += 1;
	*ctx->stack_current = value_to_push;

	log_push(state, ctx, value_to_push);
	
	return ctx->stack_current;
}

static StackValue pop_operand(ExecutorState *state, ExecutionContext *ctx)
{
	EXEC_ASSERT(state, ctx->stack_current >= ctx->stack_begin);
	if (state->failed) {
		return {};
	}

	StackValue popped = *ctx->stack_current;
	ctx->stack_current -= 1;
	log_pop(state, ctx, popped);

	return popped;
}

// -- OpCodes execution

static void execute_constant(ExecutorState *state, ExecutionContext *ctx)
{
	uint8_t i_constant = bytecode_read_u8(ctx);
	EXEC_ASSERT(state, i_constant < ctx->module->constants_u32_length);
	if (state->failed) {
		return;
	}

	uint32_t value = ctx->module->constants_u32[i_constant];

	StackValue to_push = stack_value_i32(int32_t(value));
	push_operand(state, ctx, to_push);
}

static void execute_constant_str(ExecutorState *state, ExecutionContext *ctx)
{
	uint8_t i_constant = bytecode_read_u8(ctx);
	EXEC_ASSERT(state, i_constant < ctx->module->constant_strings_length);
	if (state->failed) {
		return;
	}

	sv value = ctx->module->constant_strings[i_constant];
	
	Str str = {};
	str.is_constant = 1;
	str.offset = i_constant;
	str.length = uint32_t(value.length);
	StackValue to_push = stack_value_str(str);
	push_operand(state, ctx, to_push);
}

static void execute_call(ExecutorState *state, ExecutionContext *ctx)
{
	uint8_t i_function = bytecode_read_u8(ctx);
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--call function%d\n", i_function);

	EXEC_ASSERT(state, i_function < ctx->module->functions_length);
	if (state->failed) {
		return;
	}

	Function *function = ctx->module->functions + i_function;

	// Open a new scope for local variables
	execute_begin_scope(state, ctx);
	if (state->failed) {
		return;
	}
	
	// Push a new call frame to save return address
	EXEC_ASSERT(state, ctx->callstack_current + 1 < ctx->callstack_end);
	if (state->failed) {
		return;
	}
	
	ctx->callstack_current += 1;
	CallFrame *callstack = ctx->callstack_current;
	callstack->return_ip = ctx->ip;
	callstack->i_current_function = i_function;

	// Jump
	ctx->ip = function->address;
}

static void execute_call_foreign(ExecutorState *state, ExecutionContext *ctx)
{
	// Check that the callstack is valid
	EXEC_ASSERT(state, ctx->callstack_begin <= ctx->callstack_current && ctx->callstack_current < ctx->callstack_end);
	if (state->failed) {
		return;
	}

	CallFrame *callstack = ctx->callstack_current;

	// Check that the content of the callstack is valid
	EXEC_ASSERT(state, callstack->i_current_function < ctx->module->functions_length);
	if (state->failed) {
		return;
	}

	FunctionInstance *function = ctx->module_instance->functions + callstack->i_current_function;
	EXEC_ASSERT(state, function->foreign != nullptr);
	if (function->foreign != nullptr) {
		function->foreign(ctx);
	}
}

static void execute_ret(ExecutorState *state, ExecutionContext *ctx)
{
	execute_end_scope(state, ctx);
	
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	if (ctx->callstack_begin <= ctx->callstack_current && ctx->callstack_current < ctx->callstack_end) {
		// Pop the last callstack, restore IP
		ctx->ip = ctx->callstack_current->return_ip;
		ctx->callstack_current -= 1;
	} else {
		// There is no more callstack? Break the loop with an invalid IP value
		ctx->ip = ~uint64_t(0);
	}
}

static void execute_conditional_jump(ExecutorState *state, ExecutionContext *ctx)
{
	StackValue condition = pop_operand(state, ctx);
	if (state->failed) {
		return;
	}

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--condition_jump(%d)\n", condition.i32);

	uint32_t jump_destination = bytecode_read_u32(ctx);
	// Check that the jump destination is valid
	EXEC_ASSERT(state, jump_destination < ctx->module->bytecode_length);
	if (condition.i32 != 0) {
		ctx->ip = uint64_t(jump_destination);
	}
}

static void execute_jump(ExecutorState *state, ExecutionContext *ctx)
{
	uint32_t jump_destination = bytecode_read_u32(ctx);
	// Check that the jump destination is valid
	EXEC_ASSERT(state, jump_destination < ctx->module->bytecode_length);
	ctx->ip = uint64_t(jump_destination);
}

static void execute_store_local(ExecutorState *state, ExecutionContext *ctx)
{
	uint8_t i_local = bytecode_read_u8(ctx);
	EXEC_ASSERT(state, ctx->scopes_current >= ctx->scopes_begin);
	EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);
	if (state->failed) {
		return;
	}

	// If the value is a constant, we need to promote it in memory

	// Otherwise we need to deep copy it
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--store_local %u\n", i_local);
	// Pop the value to set
	StackValue local_value = pop_operand(state, ctx);

	// Set the local value
	ctx->scopes_current->variables[i_local] = local_value;
}

static void execute_load_local(ExecutorState *state, ExecutionContext *ctx)
{
	uint8_t i_local = bytecode_read_u8(ctx);

	EXEC_ASSERT(state, ctx->scopes_current >= ctx->scopes_begin);
	EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);
	if (state->failed) {
		return;
	}
	
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--load_local %u\n", i_local);
	push_operand(state, ctx, ctx->scopes_current->variables[i_local]);
}

static void execute_stack_alloc(ExecutorState *state, ExecutionContext *ctx)
{
	const TypeID inner_type = bytecode_read_type_id(state, ctx);
	
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--stack_alloc %u\n", inner_type.raw);

	StackValue size_to_alloc = pop_operand(state, ctx);
	
	EXEC_ASSERT(state, ctx->locals_storage_begin <= ctx->locals_storage_current);
	EXEC_ASSERT(state, ctx->locals_storage_current + size_to_alloc.i32 < ctx->locals_storage_end);
	if (state->failed) {
		return;
	}
	
	TypedPointer returned_pointer = {};
	returned_pointer.type_id = inner_type;
	returned_pointer.offset = uint32_t(ctx->locals_storage_current - ctx->locals_storage_begin);
	
	ctx->locals_storage_current += size_to_alloc.i32;

	push_operand(state, ctx, stack_value_local_ptr(returned_pointer));
}

static void execute_store(ExecutorState *state, ExecutionContext *ctx)
{
	const TypeID pointee_type = bytecode_read_type_id(state, ctx);

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--store %u\n", pointee_type.raw);

	StackValue value_to_write = pop_operand(state, ctx);
	StackValue pointer_to_write = pop_operand(state, ctx);
	uint32_t size = type_get_size(ctx->module, pointee_type);
	uint8_t *begin = ctx->locals_storage_begin + pointer_to_write.local_ptr.offset;
	uint8_t *end = begin + size;

	EXEC_ASSERT(state, pointer_to_write.kind == StackValueKind::LocalPtr); // we poped a pointer
	EXEC_ASSERT(state, ctx->locals_storage_begin <= begin && end <= ctx->locals_storage_end); // bound-check the pointer
	EXEC_ASSERT(state, type_id_is_builtin(pointer_to_write.local_ptr.type_id)); // the pointer must point to a builtin
	EXEC_ASSERT(state, pointer_to_write.local_ptr.type_id.raw == pointee_type.raw); // check the type from bytecode
	if (state->failed) {
		return;
	}

	switch (pointee_type.builtin.kind)
	{
	case BuiltinTypeKind::Unit: break;
	case BuiltinTypeKind::Int: memcpy(begin, &value_to_write.i32, sizeof(value_to_write.i32)); break;
	case BuiltinTypeKind::Bool: memcpy(begin, &value_to_write.b8, sizeof(value_to_write.b8)); break;
	case BuiltinTypeKind::Float: memcpy(begin, &value_to_write.f32, sizeof(value_to_write.f32)); break;
	case BuiltinTypeKind::Pointer: memcpy(begin, &value_to_write.local_ptr, sizeof(value_to_write.local_ptr)); break;
	case BuiltinTypeKind::Str: memcpy(begin, &value_to_write.str, sizeof(value_to_write.str)); break;
	case BuiltinTypeKind::Count: break;
	}
}

static void execute_load(ExecutorState *state, ExecutionContext *ctx)
{
	const TypeID pointee_type = bytecode_read_type_id(state, ctx);

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--load %u\n", pointee_type.raw);
	
	StackValue pointer = pop_operand(state, ctx);
	EXEC_ASSERT(state, pointer.kind == StackValueKind::LocalPtr);
	if (state->failed) {
		return;
	}

	uint32_t size = type_get_size(ctx->module, pointer.local_ptr.type_id);
	uint8_t *begin = ctx->locals_storage_begin + pointer.local_ptr.offset;
	uint8_t *end = begin + size;
	
	EXEC_ASSERT(state, ctx->locals_storage_begin <= begin && end <= ctx->locals_storage_end); // bound-check the pointer
	EXEC_ASSERT(state, type_id_is_builtin(pointer.local_ptr.type_id)); // the pointer must point to a builtin
	EXEC_ASSERT(state, pointee_type.raw == pointer.local_ptr.type_id.raw); // check bytecode type
	if (state->failed) {
		return;
	}

	StackValue result = {};
	switch (pointer.local_ptr.type_id.builtin.kind)
	{
	case BuiltinTypeKind::Unit: break;
	case BuiltinTypeKind::Int: memcpy(&result.i32, begin, sizeof(result.i32)); break;
	case BuiltinTypeKind::Bool: memcpy(&result.b8, begin, sizeof(result.b8)); break;
	case BuiltinTypeKind::Float: memcpy(&result.f32, begin, sizeof(result.f32)); break;
	case BuiltinTypeKind::Pointer: memcpy(&result.local_ptr, begin, sizeof(result.local_ptr)); break;
	case BuiltinTypeKind::Str: memcpy(&result.str, begin, sizeof(result.str)); break;
	case BuiltinTypeKind::Count: break;
	}
	push_operand(state, ctx, result);
}

template <typename Lambda>
static void execute_binop(ExecutorState *state, ExecutionContext *ctx, Lambda &&lambda)
{
	EXEC_ASSERT(state, ctx->stack_current >= ctx->stack_begin + 1);
	if (state->failed) {
		return;
	}

	// TODO: Fetch integers from memory
	StackValue rhs = *ctx->stack_current;
	ctx->stack_current -= 1;
	StackValue lhs = *ctx->stack_current;

	log_pop(state, ctx, rhs);
	log_pop(state, ctx, lhs);

	lambda(lhs, rhs);

	log_push(state, ctx, *ctx->stack_current);
}

static void execute_iadd(ExecutorState *state, ExecutionContext *ctx)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--iadd\n");
	execute_binop(state, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 + rhs.i32; });
}

static void execute_isub(ExecutorState *state, ExecutionContext *ctx)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--isub\n");
	execute_binop(state, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 - rhs.i32; });
}

static void execute_iless_than_eq(ExecutorState *state, ExecutionContext *ctx)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--iless_than_eq\n");
	execute_binop(state, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 < rhs.i32; });
}

static void execute_ptr_offset(ExecutorState *state, ExecutionContext *ctx)
{
	const TypeID return_type = bytecode_read_type_id(state, ctx);

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--ptr_offset\n");
	execute_binop(state, ctx, [&](StackValue lhs, StackValue rhs) {

		EXEC_ASSERT(state, lhs.kind == StackValueKind::LocalPtr);
		EXEC_ASSERT(state, rhs.kind == StackValueKind::I32);

		TypedPointer p = {};
		p.type_id = type_id_deref_pointer(return_type);
		p.offset = lhs.local_ptr.offset + uint32_t(rhs.i32);
		*ctx->stack_current = stack_value_local_ptr(p);
	});
}

static void execute_halt(ExecutorState *state, ExecutionContext *ctx)
{
	EXEC_ASSERT(state, false);
	state->error_msg = sv_from_null_terminated("Halt");
}

static void execute_debug_label(ExecutorState *, ExecutionContext *ctx)
{
	sv label = bytecode_read_sv(ctx);
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("\"%.*s\"\n", int(label.length), label.chars);
}

static void executor_execute_module_at(ExecutorState *state, uint64_t i_module, uint64_t first_ip)
{
	state->failed = false;
	state->error_msg = {};
	state->error_file = {};
	state->error_line = {};

	ExecutionContext ctx = {};
	ctx.module = state->modules + i_module;
	ctx.module_instance = state->module_instances + i_module;
	ctx.ip = first_ip;
	execution_init(&ctx);
	
	// Create the first variable scope
	execute_begin_scope(state, &ctx);

	Module *module = ctx.module;

	using ExecuteOpCode = void (*)(ExecutorState *, ExecutionContext *);
	ExecuteOpCode execute_opcodes[] = {
		execute_constant,
		execute_constant_str,
		execute_call,
		execute_call_foreign,
		execute_ret,
		execute_conditional_jump,
		execute_jump,
		execute_halt,
		execute_store_local,
		execute_load_local,
		execute_stack_alloc,
		execute_load,
		execute_store,
		execute_iadd,
		execute_isub,
		execute_iless_than_eq,
		execute_ptr_offset,
		execute_debug_label,
	};
	static_assert(ARRAY_LENGTH(execute_opcodes) == uint8_t(OpCodeKind::Count));

	const uint64_t bytecode_len = module->bytecode_length;
	while (ctx.ip < bytecode_len) {
		if (state->failed) {
			break;
		}

		uint8_t opcode = module->bytecode[ctx.ip];
		ctx.ip += 1;

		if (opcode >= uint8_t(OpCodeKind::Count)) {
			fprintf(stderr, "Invalid opcode %u\n", opcode);
			break;
		}

		OpCodeKind opcode_kind = OpCodeKind(opcode);
		execute_opcodes[uint8_t(opcode_kind)](state, &ctx);
	}

	// Call user callback if the execution failed
	if (state->failed && state->config.error_callback != nullptr) {
		RuntimeError err = {};
		err.message = state->error_msg;
		err.file = state->error_file;
		err.line = state->error_line;
		err.ip = state->error_ip;
		state->config.error_callback(state, err);
	}

	// DEBUG: Display stack at the end of execution
	fprintf(stdout, "End of execution. Stack:\n");
	uint32_t i = 0;
	for (StackValue *s = ctx.stack_begin; s <= ctx.stack_current; ++s) {
		fprintf(stdout, "Stack(%u) = %d\n", i, s->i32);
		i += 1;
	}
}

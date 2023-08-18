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

void executor_assert_trigger(ExecutorState *state, sv condition_str, sv file, int line)
{
	// Don't hide errors
	if (state->failed == true) {
		return;
	}

	state->failed = true;
	state->error_msg = condition_str;
	state->error_file = file;
	state->error_line = line;
}

#define EXEC_ASSERT(state, condition)                                                                                  \
	if ((condition) == false) {                                                                                        \
		executor_assert_trigger(state,                                                                                 \
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

static uint32_t type_get_size(ExecutorState *state, ExecutionContext *ctx, TypeID id)
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

static uint32_t bytecode_read_u32(ExecutionContext *ctx)
{
	uint64_t bytecode_len = ctx->module->bytecode_length;
	uint8_t *bytecode = ctx->module->bytecode;

	if (ctx->ip + sizeof(uint32_t) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read u32)\n");
		ctx->ip = bytecode_len;
		return 0;
	}
	uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(bytecode + ctx->ip);
	ctx->ip = ctx->ip + sizeof(uint32_t);
	return *bytecode_u32;
}

static int32_t bytecode_read_i32(ExecutionContext *ctx)
{
	uint64_t bytecode_len = ctx->module->bytecode_length;
	uint8_t *bytecode = ctx->module->bytecode;

	if (ctx->ip + sizeof(int32_t) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read i32)\n");
		ctx->ip = bytecode_len;
		return 0;
	}
	int32_t *bytecode_i32 = reinterpret_cast<int32_t *>(bytecode + ctx->ip);
	ctx->ip = ctx->ip + sizeof(int32_t);
	return *bytecode_i32;
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

// Returns a pointer to the pushed value
uint8_t *execution_push_local(ExecutionContext *ctx, uint64_t local_size)
{
	uint8_t *value_end = ctx->locals_storage_current + local_size;
	if (value_end <= ctx->locals_storage_end) {
		uint8_t *value_start = ctx->locals_storage_current;
		ctx->locals_storage_previous = ctx->locals_storage_current;
		ctx->locals_storage_current = value_end;
		return value_start;
	}
	// TODO: error handling
	return nullptr;
}

StackValue execution_pop_value(ExecutionContext *ctx)
{
	if (ctx->locals_storage_current != ctx->locals_storage_previous) {
		StackValue *memory_value = reinterpret_cast<StackValue *>(ctx->locals_storage_current);
		ctx->locals_storage_current = ctx->locals_storage_previous;
		return *memory_value;
	}
	// TODO: error handling
	return {};
}

StackValue execution_get_local(ExecutionContext *ctx, uint32_t i_local)
{
	return ctx->scopes_current->variables[i_local];
}

sv execution_get_str(ExecutionContext *ctx, Str str)
{
	if (str.is_constant == 1) {
		if (str.offset < ctx->module->constants_length) {
			return ctx->module->constants[str.offset].str;
		}
	}
	return sv_from_null_terminated("<ERROR>");
}

static void print_indent(long long n)
{
	for (int32_t i = 0; i < n; ++i) {
		putchar(' ');
		putchar(' ');
		putchar(' ');
		putchar(' ');
	}
}

StackValue *push_operand(ExecutorState *state, ExecutionContext *ctx, StackValue value_to_push)
{
	EXEC_ASSERT(state, ctx->stack_current + 1 < ctx->stack_end);
	if (state->failed) {
		return nullptr;
	}

	ctx->stack_current += 1;
	*ctx->stack_current = value_to_push;
	return ctx->stack_current;
}

StackValue pop_operand(ExecutorState *state, ExecutionContext *ctx)
{
	EXEC_ASSERT(state, ctx->stack_current >= ctx->stack_begin);
	if (state->failed) {
		return {};
	}

	StackValue popped = *ctx->stack_current;
	ctx->stack_current -= 1;
	return popped;
}

CallFrame *push_callstack(ExecutorState *state, ExecutionContext *ctx)
{
	EXEC_ASSERT(state, ctx->callstack_current + 1 < ctx->callstack_end);
	if (state->failed) {
		return nullptr;
	}
	ctx->callstack_current += 1;
	return ctx->callstack_current;
}

// -- OpCodes execution

static void execute_constant(ExecutorState *state, ExecutionContext *ctx)
{
	int32_t value = bytecode_read_i32(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Constant: pushed %d\n", value);

	StackValue to_push = stack_value_i32(value);
	push_operand(state, ctx, to_push);
}

static void execute_constant_str(ExecutorState *state, ExecutionContext *ctx)
{
	uint32_t i_constant = bytecode_read_u32(ctx);
	EXEC_ASSERT(state, i_constant < ctx->module->constants_length);
	if (state->failed) {
		return;
	}

	sv value = ctx->module->constants[i_constant].str;
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("ConstantStr: pushed \"%.*s\"\n", int(value.length), value.chars);

	Str str = {};
	str.is_constant = 1;
	str.offset = i_constant;
	str.length = uint32_t(value.length);
	StackValue to_push = stack_value_str(str);
	push_operand(state, ctx, to_push);
}

static void execute_call(ExecutorState *state, ExecutionContext *ctx)
{
	uint32_t i_function = bytecode_read_u32(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Call function %d\n", i_function);

	EXEC_ASSERT(state, i_function < ctx->module->functions_length);
	if (state->failed) {
		return;
	}

	Function *function = ctx->module->functions + i_function;
	CallFrame *callstack = push_callstack(state, ctx);
	if (callstack == nullptr) {
		return;
	}

	// Push a new call frame with the saved up return address
	callstack->return_ip = ctx->ip;
	callstack->i_current_function = i_function;

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

static void execute_ret(ExecutorState *, ExecutionContext *ctx)
{
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	if (ctx->callstack_begin <= ctx->callstack_current && ctx->callstack_current < ctx->callstack_end) {
		// Pop the last callstack, restore IP
		ctx->ip = ctx->callstack_current->return_ip;
		ctx->callstack_current -= 1;
	} else {
		// There is no more callstack? Break the loop with an invalid IP value
		ctx->ip = ~uint64_t(0);
	}
	printf("<- Return to %zu\n", ctx->ip);
}

static void execute_conditional_jump(ExecutorState *state, ExecutionContext *ctx)
{
	StackValue condition = pop_operand(state, ctx);
	if (state->failed) {
		return;
	}

	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Popped %d\n", condition.i32);

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

static void execute_get_field(ExecutorState *state, ExecutionContext *ctx)
{
	Module *module = ctx->module;

	uint32_t i_field = bytecode_read_u32(ctx);
	StackValue struct_ptr = pop_operand(state, ctx);
	TypeID struct_type_id = struct_ptr.local_ptr.type_id;
	if (state->failed) {
		return;
	}

	// Debug print
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("GetField #%u (struct at %u type %u)\n",
		i_field,
		struct_ptr.local_ptr.offset,
		struct_ptr.local_ptr.type_id.raw);

	// Check thast the struct ptr is valid
	EXEC_ASSERT(state, struct_ptr.local_ptr.offset < uint32_t(ctx->locals_storage_end - ctx->locals_storage_begin));
	EXEC_ASSERT(state, type_id_is_user_defined(struct_type_id));
	EXEC_ASSERT(state, struct_type_id.user_defined.index < ctx->module->types_length);
	if (state->failed) {
		return;
	}

	UserDefinedType *struct_type = module->types + struct_type_id.user_defined.index;

	// Assert that the field is valid
	EXEC_ASSERT(state, i_field < struct_type->field_count);
	if (state->failed) {
		return;
	}

	uint64_t field_offset = struct_type->field_offsets[i_field];
	TypeID field_type = struct_type->field_types[i_field];

	// Check that the user defined type is valid
	if (type_id_is_user_defined(field_type)) {
		EXEC_ASSERT(state, field_type.user_defined.index < module->types_length);
		if (state->failed) {
			return;
		}
	}

	// Push the member by value (addressable local object)
	TypedPointer local_ptr = {};
	local_ptr.type_id = field_type;
	local_ptr.offset = uint32_t(struct_ptr.local_ptr.offset + field_offset);
	StackValue field_ptr = stack_value_local_object(local_ptr);
	push_operand(state, ctx, field_ptr);

	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Field value: ptr(struct at offset %u type %u)\n",
		field_ptr.local_ptr.offset,
		field_ptr.local_ptr.type_id.raw);
}

static void execute_set_field(ExecutorState *state, ExecutionContext *ctx)
{
	Module *module = ctx->module;

	uint32_t i_field = bytecode_read_u32(ctx);

	StackValue field_value = pop_operand(state, ctx);
	StackValue struct_ptr = pop_operand(state, ctx);
	TypeID struct_type_id = struct_ptr.local_ptr.type_id;
	if (state->failed) {
		return;
	}

	// Debug print
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("SetField #%u (struct at %u type %u)\n", i_field, struct_ptr.local_ptr.offset, struct_type_id.raw);
	printf("Field value: int(%u) ptr(struct at %u type %u)\n",
		field_value.i32,
		field_value.local_ptr.offset,
		field_value.local_ptr.type_id.raw);

	// Check thast the struct ptr is valid
	EXEC_ASSERT(state, struct_ptr.local_ptr.offset < uint32_t(ctx->locals_storage_end - ctx->locals_storage_begin));
	EXEC_ASSERT(state, type_id_is_user_defined(struct_type_id));
	EXEC_ASSERT(state, struct_type_id.user_defined.index < ctx->module->types_length);
	if (state->failed) {
		return;
	}

	UserDefinedType *struct_type = module->types + struct_type_id.user_defined.index;
	uint8_t *struct_pointer = ctx->locals_storage_begin + struct_ptr.local_ptr.offset;

	// Assert that the field is valid
	EXEC_ASSERT(state, i_field < struct_type->field_count);
	if (state->failed) {
		return;
	}

	TypeID field_type_id = struct_type->field_types[i_field];
	uint8_t *field_pointer = struct_pointer + struct_type->field_offsets[i_field];

	if (type_id_is_user_defined(field_type_id)) {
		// Check that the pointer on the stack is valid.
		EXEC_ASSERT(state, field_type_id.user_defined.index < module->types_length);
		EXEC_ASSERT(state, field_type_id.raw == field_value.local_ptr.type_id.raw);
		EXEC_ASSERT(state,
			field_value.local_ptr.offset < uint32_t(ctx->locals_storage_end - ctx->locals_storage_begin));
		if (state->failed) {
			return;
		}
	}

	uint32_t field_size = type_get_size(state, ctx, field_type_id);
	memcpy(field_pointer, ctx->locals_storage_begin + field_value.local_ptr.offset, field_size);
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

static void execute_set_local(ExecutorState *state, ExecutionContext *ctx)
{
	uint32_t i_local = bytecode_read_u32(ctx);
	EXEC_ASSERT(state, ctx->scopes_current >= ctx->scopes_begin);
	EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);
	if (state->failed) {
		return;
	}

	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Popped %d (local #%u)\n", ctx->stack_current->i32, i_local);

	// If the value is a constant, we need to promote it in memory

	// Otherwise we need to deep copy it

	// Pop the value to set
	StackValue local_value = pop_operand(state, ctx);

	// Set the local value
	ctx->scopes_current->variables[i_local] = local_value;
}

static void execute_get_local(ExecutorState *state, ExecutionContext *ctx)
{
	uint32_t i_local = bytecode_read_u32(ctx);

	EXEC_ASSERT(state, ctx->scopes_current >= ctx->scopes_begin);
	EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);
	if (state->failed) {
		return;
	}

	StackValue *pushed = push_operand(state, ctx, ctx->scopes_current->variables[i_local]);

	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Pushed %d (local #%u)\n", pushed->i32, i_local);
}

static void execute_make_struct(ExecutorState *state, ExecutionContext *ctx)
{
	// Get the type from the bytecode
	TypeID struct_type_id = bytecode_read_type_id(state, ctx);
	EXEC_ASSERT(state, type_id_is_user_defined(struct_type_id));
	if (state->failed) {
		return;
	}
	UserDefinedType *struct_type = ctx->module->types + struct_type_id.user_defined.index;

	// Reserve space in the local storage for the struct data
	uint8_t *local_struct_storage = execution_push_local(ctx, struct_type->size);
	EXEC_ASSERT(state, local_struct_storage != nullptr);
	if (state->failed) {
		return;
	}

	// TODO: Keep a pointer to pop this on scope end
	StackValue new_struct = {};
	new_struct.kind = StackValueKind::LocalPtr;
	new_struct.local_ptr.type_id = struct_type_id;
	new_struct.local_ptr.offset = uint32_t(local_struct_storage - ctx->locals_storage_begin);
	push_operand(state, ctx, new_struct);

	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Pushed struct (type %u) (offset %u)\n", struct_type_id.raw, ctx->stack_current->i32);
}

static void execute_addr(ExecutorState *state, ExecutionContext *ctx)
{
	const TypeID inner_type = bytecode_read_type_id(state, ctx);
	StackValue operand = pop_operand(state, ctx);

	// It's not valid to take the address of a constant
	EXEC_ASSERT(state, !stack_value_is_constant(operand));
	if (state->failed) {
		return;
	}
	// We have either an object or a ptr

	// Verify that the type of the operand matches the type of the addr return type
	EXEC_ASSERT(state, operand.local_ptr.type_id.raw == inner_type.raw);

	// Because we can only take the address of addressable objects,
	// and that addressable objects are represented by their address in the operand stack,
	// the value on the stack is already a pointer.
	operand.kind = StackValueKind::LocalPtr;
	push_operand(state, ctx, operand);
}

static void execute_deref(ExecutorState *state, ExecutionContext *ctx)
{
	const TypeID pointer_type = bytecode_read_type_id(state, ctx);
	StackValue operand = pop_operand(state, ctx);

	EXEC_ASSERT(state, operand.kind == StackValueKind::LocalPtr);
	if (state->failed) {
		return;
	}

	// Verify that the type of the operand matches the type of the addr return type
	EXEC_ASSERT(state, operand.local_ptr.type_id.raw == pointer_type.raw);

	// Local pointer and addressable objects are represented the same way
	operand.kind = StackValueKind::LocalObject;
	push_operand(state, ctx, operand);
}

static void execute_write_i32(ExecutorState *state, ExecutionContext *ctx)
{
	StackValue value_to_write = pop_operand(state, ctx);
	StackValue pointer_to_write = pop_operand(state, ctx);
	uint8_t *address = ctx->locals_storage_begin + pointer_to_write.local_ptr.offset;

	// Check the type of operands and that the pointer is valid
	EXEC_ASSERT(state, value_to_write.kind == StackValueKind::I32);
	EXEC_ASSERT(state, pointer_to_write.kind == StackValueKind::LocalPtr);
	EXEC_ASSERT(state, address + sizeof(value_to_write.i32) <= ctx->locals_storage_end);
	if (type_id_is_user_defined(pointer_to_write.local_ptr.type_id)) {
		EXEC_ASSERT(state, pointer_to_write.local_ptr.type_id.user_defined.index < ctx->module->types_length);
	}
	if (state->failed) {
		return;
	}

	memcpy(address, &value_to_write.i32, sizeof(value_to_write.i32));
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

	lambda(lhs, rhs);

	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("Popped, Popped, Pushed %d\n", ctx->stack_current->i32);
}

static void execute_iadd(ExecutorState *state, ExecutionContext *ctx)
{
	execute_binop(state, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 + rhs.i32; });
}

static void execute_isub(ExecutorState *state, ExecutionContext *ctx)
{
	execute_binop(state, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 - rhs.i32; });
}
static void execute_iless_than_eq(ExecutorState *state, ExecutionContext *ctx)
{
	execute_binop(state, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 < rhs.i32; });
}
static void execute_halt(ExecutorState *state, ExecutionContext *)
{
	EXEC_ASSERT(state, false);
	state->error_msg = sv_from_null_terminated("Halt");
}
static void execute_debug_label(ExecutorState *, ExecutionContext *ctx)
{
	sv label = bytecode_read_sv(ctx);
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
		execute_get_field,
		execute_set_field,
		execute_begin_scope,
		execute_end_scope,
		execute_set_local,
		execute_get_local,
		execute_make_struct,
		execute_addr,
		execute_deref,
		execute_write_i32,
		execute_iadd,
		execute_isub,
		execute_iless_than_eq,
		execute_halt,
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

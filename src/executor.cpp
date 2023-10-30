#include "executor.h"
#include "module.h"
#include "vm.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

inline constexpr uint32_t VARSCOPE_CAPACITY = 16;

#define EXEC_ASSERT(ctx, cond) ERROR_ASSERT((&(ctx)->error), cond, ErrorCode::Assert);

struct CallFrame
{
	uint32_t i_module;
	uint32_t i_current_function;
	uint32_t return_ip;
};

struct VariableScope
{
	StackValue variables[VARSCOPE_CAPACITY];
	uint32_t variables_length;
};

struct ExecutionContext
{
	VM *vm;

	Error error;

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

	uint32_t i_current_module;
	uint32_t ip;
};

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

static Module *execution_current_module(ExecutionContext *context)
{
	return vec_at(&context->vm->modules, context->i_current_module);
}

static ModuleInstance *execution_current_module_instance(ExecutionContext *context)
{
	return vec_at(&context->vm->module_instances, context->i_current_module);
}

uint32_t type_get_size(ExecutionContext *ctx, TypeID id)
{
	Module *module = execution_current_module(ctx);
	if (id.builtin.is_user_defined != 0) {
		EXEC_ASSERT(ctx, id.user_defined.index < module->types_length);
		if (ctx->error.code != ErrorCode::Ok) {
			return 0;
		}
		return module->types[id.user_defined.index].size;
	} else {
		return BuiltinTypeKind_size[uint32_t(id.builtin.kind)];
	}
}

static TypeID bytecode_read_type_id(ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	uint32_t bytecode_len = module->bytecode_length;
	uint8_t *bytecode = module->bytecode;

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
		EXEC_ASSERT(ctx, id.builtin.kind < BuiltinTypeKind::Count);
	} else {
		EXEC_ASSERT(ctx, type_id_is_user_defined(id));
		EXEC_ASSERT(ctx, id.user_defined.index < module->types_length);
	}

	return id;
}

template <typename T>
static T bytecode_read_scalar(ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	uint32_t bytecode_len = module->bytecode_length;
	uint8_t *bytecode = module->bytecode;

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
	Module *module = execution_current_module(ctx);
	uint32_t bytecode_len = module->bytecode_length;
	uint8_t *bytecode = module->bytecode;
	uint32_t local_ip = ctx->ip;

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

static void execute_begin_scope(VM *, ExecutionContext *ctx)
{
	EXEC_ASSERT(ctx, ctx->scopes_current < ctx->scopes_end);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}
	ctx->scopes_current += 1;
	ctx->scopes_current->variables_length = 0;
	memset(ctx->scopes_current->variables, 0, sizeof(ctx->scopes_current->variables));
}

static void execute_end_scope(VM *, ExecutionContext *ctx)
{
	EXEC_ASSERT(ctx, ctx->scopes_current >= ctx->scopes_begin);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}
	ctx->scopes_current -= 1;
}

static void print_ip(ExecutionContext *ctx)
{
	printf("%u\t| ", ctx->ip);
}

static void print_indent(long long n)
{
	for (int32_t i = 0; i < (n + 1); ++i) {
		putchar(' ');
		putchar(' ');
		putchar(' ');
		putchar(' ');
	}
}

static void log_push(VM *, ExecutionContext *ctx, StackValue value)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("push %s %d\n", StackValueKind_str[uint8_t(value.kind)], value.i32);
}

static void log_pop(VM *, ExecutionContext *ctx, StackValue value)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("pop  %s %d\n", StackValueKind_str[uint8_t(value.kind)], value.i32);
}

static StackValue *push_operand(VM *vm, ExecutionContext *ctx, StackValue value_to_push)
{
	EXEC_ASSERT(ctx, ctx->stack_current + 1 < ctx->stack_end);
	if (ctx->error.code != ErrorCode::Ok) {
		return nullptr;
	}

	ctx->stack_current += 1;
	*ctx->stack_current = value_to_push;

	log_push(vm, ctx, value_to_push);

	return ctx->stack_current;
}

static StackValue pop_operand(VM *vm, ExecutionContext *ctx)
{
	EXEC_ASSERT(ctx, ctx->stack_current >= ctx->stack_begin);
	if (ctx->error.code != ErrorCode::Ok) {
		return {};
	}

	StackValue popped = *ctx->stack_current;
	ctx->stack_current -= 1;
	log_pop(vm, ctx, popped);

	return popped;
}

// -- OpCodes execution

static void execute_constant(VM *vm, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	uint8_t i_constant = bytecode_read_u8(ctx);
	EXEC_ASSERT(ctx, i_constant < module->constants_u32_length);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	uint32_t value = module->constants_u32[i_constant];

	StackValue to_push = stack_value_i32(int32_t(value));
	push_operand(vm, ctx, to_push);
}

static void execute_constant_str(VM *vm, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	uint8_t i_constant = bytecode_read_u8(ctx);
	EXEC_ASSERT(ctx, i_constant < module->constant_strings_length);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	sv value = module->constant_strings[i_constant];

	Str str = {};
	str.is_constant = 1;
	str.offset = i_constant;
	str.length = uint32_t(value.length);
	StackValue to_push = stack_value_str(str);
	push_operand(vm, ctx, to_push);
}

static void execute_call(VM *vm, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	uint8_t i_function = bytecode_read_u8(ctx);
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--call function%d\n", i_function);

	EXEC_ASSERT(ctx, i_function < module->functions_length);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	Function *function = module->functions + i_function;

	// Open a new scope for local variables
	execute_begin_scope(vm, ctx);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	// Push a new call frame to save return address
	EXEC_ASSERT(ctx, ctx->callstack_current + 1 < ctx->callstack_end);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	ctx->callstack_current += 1;
	CallFrame *callstack = ctx->callstack_current;
	callstack->i_module = ctx->i_current_module;
	callstack->return_ip = ctx->ip;
	callstack->i_current_function = i_function;

	// Jump
	ctx->ip = function->address;
}

static void execute_call_external(VM *vm, ExecutionContext *ctx)
{
	uint8_t i_module = bytecode_read_u8(ctx);
	uint8_t i_function = bytecode_read_u8(ctx);
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--call-external module%u function%u\n", i_module, i_function);

	EXEC_ASSERT(ctx, i_module < vm->modules.length);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	Module *module = vec_at(&vm->modules, i_module);
	EXEC_ASSERT(ctx, i_function < module->functions_length);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	Function *function = module->functions + i_function;

	// Open a new scope for local variables
	execute_begin_scope(vm, ctx);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	// Push a new call frame to save return address
	EXEC_ASSERT(ctx, ctx->callstack_current + 1 < ctx->callstack_end);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	ctx->callstack_current += 1;
	CallFrame *callstack = ctx->callstack_current;
	callstack->i_module = ctx->i_current_module;
	callstack->return_ip = ctx->ip;
	callstack->i_current_function = i_function;

	// Jump
	ctx->i_current_module = i_module;
	ctx->ip = function->address;
}

static void execute_call_foreign(VM *vm, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	ModuleInstance *module_instance = execution_current_module_instance(ctx);
	// Check that the callstack is valid
	EXEC_ASSERT(ctx, ctx->callstack_begin <= ctx->callstack_current && ctx->callstack_current < ctx->callstack_end);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	CallFrame *callstack = ctx->callstack_current;

	// Check that the content of the callstack is valid
	EXEC_ASSERT(ctx, callstack->i_current_function < module->functions_length);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	FunctionInstance *function = module_instance->functions + callstack->i_current_function;
	EXEC_ASSERT(ctx, function->foreign != nullptr);
	if (function->foreign != nullptr) {
		function->foreign(vm);
	}
}

static void execute_ret(VM *vm, ExecutionContext *ctx)
{
	execute_end_scope(vm, ctx);

	print_indent(ctx->callstack_current - ctx->callstack_begin);
	if (ctx->callstack_begin <= ctx->callstack_current && ctx->callstack_current < ctx->callstack_end) {
		// Pop the last callstack, restore IP
		ctx->i_current_module = ctx->callstack_current->i_module;
		ctx->ip = ctx->callstack_current->return_ip;
		ctx->callstack_current -= 1;
	} else {
		// There is no more callstack? Break the loop with an invalid IP value
		ctx->ip = ~uint32_t(0);
	}
}

static void execute_conditional_jump(VM *vm, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	StackValue condition = pop_operand(vm, ctx);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--condition_jump(%d)\n", condition.i32);

	uint32_t jump_destination = bytecode_read_u32(ctx);
	// Check that the jump destination is valid
	EXEC_ASSERT(ctx, jump_destination < module->bytecode_length);
	if (condition.i32 != 0) {
		ctx->ip = uint32_t(jump_destination);
	}
}

static void execute_jump(VM *, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	uint32_t jump_destination = bytecode_read_u32(ctx);
	// Check that the jump destination is valid
	EXEC_ASSERT(ctx, jump_destination < module->bytecode_length);
	ctx->ip = uint32_t(jump_destination);
}

static void execute_store_local(VM *vm, ExecutionContext *ctx)
{
	uint8_t i_local = bytecode_read_u8(ctx);
	EXEC_ASSERT(ctx, ctx->scopes_current >= ctx->scopes_begin);
	EXEC_ASSERT(ctx, i_local < VARSCOPE_CAPACITY);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	// If the value is a constant, we need to promote it in memory

	// Otherwise we need to deep copy it
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--store_local %u\n", i_local);
	// Pop the value to set
	StackValue local_value = pop_operand(vm, ctx);

	// Set the local value
	ctx->scopes_current->variables[i_local] = local_value;
}

static void execute_load_local(VM *vm, ExecutionContext *ctx)
{
	uint8_t i_local = bytecode_read_u8(ctx);

	EXEC_ASSERT(ctx, ctx->scopes_current >= ctx->scopes_begin);
	EXEC_ASSERT(ctx, i_local < VARSCOPE_CAPACITY);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--load_local %u\n", i_local);
	push_operand(vm, ctx, ctx->scopes_current->variables[i_local]);
}

static void execute_stack_alloc(VM *vm, ExecutionContext *ctx)
{
	const TypeID inner_type = bytecode_read_type_id(ctx);

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--stack_alloc %u\n", inner_type.raw);

	StackValue size_to_alloc = pop_operand(vm, ctx);

	EXEC_ASSERT(ctx, ctx->locals_storage_begin <= ctx->locals_storage_current);
	EXEC_ASSERT(ctx, ctx->locals_storage_current + size_to_alloc.i32 < ctx->locals_storage_end);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	TypedPointer returned_pointer = {};
	returned_pointer.type_id = inner_type;
	returned_pointer.offset = uint32_t(ctx->locals_storage_current - ctx->locals_storage_begin);

	ctx->locals_storage_current += size_to_alloc.i32;

	push_operand(vm, ctx, stack_value_local_ptr(returned_pointer));
}

static void execute_store(VM *vm, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	const TypeID pointee_type = bytecode_read_type_id(ctx);

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--store %u\n", pointee_type.raw);

	StackValue value_to_write = pop_operand(vm, ctx);
	StackValue pointer_to_write = pop_operand(vm, ctx);
	uint32_t size = type_get_size(module, pointee_type);
	uint8_t *begin = ctx->locals_storage_begin + pointer_to_write.local_ptr.offset;
	uint8_t *end = begin + size;

	EXEC_ASSERT(ctx, pointer_to_write.kind == StackValueKind::LocalPtr);                    // we poped a pointer
	EXEC_ASSERT(ctx, ctx->locals_storage_begin <= begin && end <= ctx->locals_storage_end); // bound-check the pointer
	EXEC_ASSERT(ctx, type_id_is_builtin(pointer_to_write.local_ptr.type_id));     // the pointer must point to a builtin
	EXEC_ASSERT(ctx, pointer_to_write.local_ptr.type_id.raw == pointee_type.raw); // check the type from bytecode
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	switch (pointee_type.builtin.kind) {
	case BuiltinTypeKind::Unit:
		break;
	case BuiltinTypeKind::Int:
		memcpy(begin, &value_to_write.i32, sizeof(value_to_write.i32));
		break;
	case BuiltinTypeKind::Bool:
		memcpy(begin, &value_to_write.b8, sizeof(value_to_write.b8));
		break;
	case BuiltinTypeKind::Float:
		memcpy(begin, &value_to_write.f32, sizeof(value_to_write.f32));
		break;
	case BuiltinTypeKind::Pointer:
		memcpy(begin, &value_to_write.local_ptr, sizeof(value_to_write.local_ptr));
		break;
	case BuiltinTypeKind::Str:
		memcpy(begin, &value_to_write.str, sizeof(value_to_write.str));
		break;
	case BuiltinTypeKind::Count:
		break;
	}
}

static void execute_load(VM *vm, ExecutionContext *ctx)
{
	Module *module = execution_current_module(ctx);
	const TypeID pointee_type = bytecode_read_type_id(ctx);

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--load %u\n", pointee_type.raw);

	StackValue pointer = pop_operand(vm, ctx);
	EXEC_ASSERT(ctx, pointer.kind == StackValueKind::LocalPtr);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	uint32_t size = type_get_size(module, pointer.local_ptr.type_id);
	uint8_t *begin = ctx->locals_storage_begin + pointer.local_ptr.offset;
	uint8_t *end = begin + size;

	EXEC_ASSERT(ctx, ctx->locals_storage_begin <= begin && end <= ctx->locals_storage_end); // bound-check the pointer
	EXEC_ASSERT(ctx, type_id_is_builtin(pointer.local_ptr.type_id));     // the pointer must point to a builtin
	EXEC_ASSERT(ctx, pointee_type.raw == pointer.local_ptr.type_id.raw); // check bytecode type
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	StackValue result = {};
	switch (pointer.local_ptr.type_id.builtin.kind) {
	case BuiltinTypeKind::Unit:
		break;
	case BuiltinTypeKind::Int:
		memcpy(&result.i32, begin, sizeof(result.i32));
		break;
	case BuiltinTypeKind::Bool:
		memcpy(&result.b8, begin, sizeof(result.b8));
		break;
	case BuiltinTypeKind::Float:
		memcpy(&result.f32, begin, sizeof(result.f32));
		break;
	case BuiltinTypeKind::Pointer:
		memcpy(&result.local_ptr, begin, sizeof(result.local_ptr));
		break;
	case BuiltinTypeKind::Str:
		memcpy(&result.str, begin, sizeof(result.str));
		break;
	case BuiltinTypeKind::Count:
		break;
	}
	push_operand(vm, ctx, result);
}

template <typename Lambda>
static void execute_binop(VM *vm, ExecutionContext *ctx, Lambda &&lambda)
{
	EXEC_ASSERT(ctx, ctx->stack_current >= ctx->stack_begin + 1);
	if (ctx->error.code != ErrorCode::Ok) {
		return;
	}

	StackValue rhs = *ctx->stack_current;
	ctx->stack_current -= 1;
	StackValue lhs = *ctx->stack_current;

	log_pop(vm, ctx, rhs);
	log_pop(vm, ctx, lhs);

	lambda(lhs, rhs);

	log_push(vm, ctx, *ctx->stack_current);
}

static void execute_iadd(VM *vm, ExecutionContext *ctx)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--iadd\n");
	execute_binop(vm, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 + rhs.i32; });
}

static void execute_isub(VM *vm, ExecutionContext *ctx)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--isub\n");
	execute_binop(vm, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 - rhs.i32; });
}

static void execute_iless_than_eq(VM *vm, ExecutionContext *ctx)
{
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--iless_than_eq\n");
	execute_binop(vm, ctx, [ctx](StackValue lhs, StackValue rhs) { ctx->stack_current->i32 = lhs.i32 < rhs.i32; });
}

static void execute_ptr_offset(VM *vm, ExecutionContext *ctx)
{
	const TypeID return_type = bytecode_read_type_id(ctx);

	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("--ptr_offset\n");
	execute_binop(vm, ctx, [&](StackValue lhs, StackValue rhs) {
		EXEC_ASSERT(ctx, lhs.kind == StackValueKind::LocalPtr);
		EXEC_ASSERT(ctx, rhs.kind == StackValueKind::I32);

		TypedPointer p = {};
		p.type_id = type_id_deref_pointer(return_type);
		p.offset = lhs.local_ptr.offset + uint32_t(rhs.i32);
		*ctx->stack_current = stack_value_local_ptr(p);
	});
}

static void execute_halt(VM *, ExecutionContext *ctx)
{
	EXEC_ASSERT(ctx, false);
	ctx->error.msg = sv_from_null_terminated("Halt");
}

static void execute_debug_label(VM *, ExecutionContext *ctx)
{
	sv label = bytecode_read_sv(ctx);
	print_ip(ctx);
	print_indent(ctx->callstack_current - ctx->callstack_begin);
	printf("\"%.*s\"\n", int(label.length), label.chars);
}

void executor_execute_module_at(VM *vm, uint32_t i_module, uint32_t first_ip)
{
	ExecutionContext ctx = {};
	ctx.vm = vm;
	ctx.ip = first_ip;
	ctx.i_current_module = i_module;
	execution_init(&ctx);

	// Create the first variable scope
	execute_begin_scope(vm, &ctx);

	using ExecuteOpCode = void (*)(VM *, ExecutionContext *);
	ExecuteOpCode execute_opcodes[] = {
		execute_constant,
		execute_constant_str,
		execute_call,
		execute_call_external,
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

	while (true) {
		const Module *module = execution_current_module(&ctx);
		const uint32_t bytecode_len = module->bytecode_length;
		if (ctx.ip >= bytecode_len) {
			break;
		}

		if (ctx.error.code != ErrorCode::Ok) {
			break;
		}

		uint8_t opcode = module->bytecode[ctx.ip];
		ctx.ip += 1;

		if (opcode >= uint8_t(OpCodeKind::Count)) {
			fprintf(stderr, "Invalid opcode %u\n", opcode);
			break;
		}

		OpCodeKind opcode_kind = OpCodeKind(opcode);
		execute_opcodes[uint8_t(opcode_kind)](vm, &ctx);
	}

	// Call user callback if the execution failed
	if (ctx.error.code != ErrorCode::Ok && vm->config.error_callback != nullptr) {
		vm->config.error_callback(vm, ctx.error);
	}

	// DEBUG: Display stack at the end of execution
	fprintf(stdout, "End of execution. Stack:\n");
	uint32_t i = 0;
	for (StackValue *s = ctx.stack_begin; s <= ctx.stack_current; ++s) {
		fprintf(stdout, "Stack(%u) = %d\n", i, s->i32);
		i += 1;
	}
}

//  StackValue execution_get_local(VM *vm, uint32_t i_local)
// {
// 	return ctx->scopes_current->variables[i_local];
// }

//  sv execution_get_str(VM *vm, Str str)
// {
// 	if (str.is_constant == 1) {
// 		if (str.offset < module->constant_strings_length) {
// 			return module->constant_strings[str.offset];
// 		}
// 	}
// 	return sv_from_null_terminated("<ERROR>");
// }

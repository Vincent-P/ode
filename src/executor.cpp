// TODO: Set arguments to locals when calling functions

#include "executor.h"
#include "image.h"
#include <corecrt_wstdio.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct ExecutorState
{
	ExecutorConfig config;
	Image image;
	bool failed;
	sv error_msg;
	sv error_file;
	int error_line;
	uint64_t error_ip;
};

struct CallFrame
{
	uint64_t return_ip;
};

union StackValue
{
	int32_t i32;
};

#define VARSCOPE_CAPACITY 16
struct VariableScope
{
	StackValue variables[VARSCOPE_CAPACITY];
	uint64_t variables_length;
};

ExecutorState *executor_init(ExecutorConfig config)
{
	ExecutorState *state = reinterpret_cast<ExecutorState *>(calloc(1, sizeof(ExecutorState)));
	state->config = config;
	return state;
}

void executor_load_image(ExecutorState *state, Image *image)
{
	state->image = *image;
}

void executor_execute_module_at(ExecutorState *state, Module *module, uint64_t ip);
void executor_execute_module_entrypoint(ExecutorState *state, sv module_name)
{
	const uint64_t modules_len = state->image.modules_length;
	uint64_t i_module = 0;
	for (; i_module < modules_len; ++i_module) {
		if (sv_equals(state->image.modules[i_module].name, module_name)) {
			break;
		}
	}

	// Module not found
	if (i_module >= modules_len) {
		fprintf(stderr, "module \"%.*s\" not found\n", int(module_name.length), module_name.chars);
		return;
	}

	Module *module = state->image.modules + i_module;

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
	executor_execute_module_at(state, module, ip);
}

static uint32_t bytecode_read_u32(uint8_t *bytecode, uint64_t bytecode_len, uint64_t *ip)
{
	if (*ip + sizeof(uint32_t) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read u32)\n");
		*ip = bytecode_len;
		return 0;
	}
	uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(bytecode + *ip);
	*ip = *ip + sizeof(uint32_t);
	return *bytecode_u32;
}

static int32_t bytecode_read_i32(uint8_t *bytecode, uint64_t bytecode_len, uint64_t *ip)
{
	if (*ip + sizeof(int32_t) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read i32)\n");
		*ip = bytecode_len;
		return 0;
	}
	int32_t *bytecode_i32 = reinterpret_cast<int32_t *>(bytecode + *ip);
	*ip = *ip + sizeof(int32_t);
	return *bytecode_i32;
}

static sv bytecode_read_sv(uint8_t *bytecode, uint64_t bytecode_len, uint64_t *ip)
{
	uint64_t local_ip = *ip;

	// Read string length
	if (local_ip + sizeof(uint32_t) > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read sv length)\n");
		*ip = bytecode_len;
		return {};
	}
	uint32_t *bytecode_u32 = reinterpret_cast<uint32_t *>(bytecode + local_ip);
	sv result = {};
	result.length = *bytecode_u32;
	local_ip += sizeof(uint32_t);

	// Read string
	if (local_ip + result.length > bytecode_len) {
		fprintf(stderr, "Invalid bytecode (read sv chars)\n");
		*ip = bytecode_len;
		return {};
	}
	char *bytecode_char = reinterpret_cast<char *>(bytecode + local_ip);
	result.chars = bytecode_char;

	*ip = *ip + sizeof(uint32_t) + result.length;

	return result;
}

void executor_assert_trigger(ExecutorState *state, sv condition_str, sv file, int line)
{
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
#define EXEC_JUMP_IF_FAILED(state, label)                                                                              \
	if (state->failed) {                                                                                               \
		goto label;                                                                                                    \
	}

void executor_execute_module_at(ExecutorState *state, Module *module, uint64_t ip)
{
	state->failed = false;
	state->error_msg = {};
	state->error_file = {};
	state->error_line = {};

	const uint64_t bytecode_len = module->bytecode_length;

	CallFrame *const callstack_begin = static_cast<CallFrame *>(calloc(128, sizeof(VariableScope)));
	CallFrame *callstack_current = callstack_begin - 1;
	const CallFrame *const callstack_end = callstack_begin + 128;

	VariableScope *const scopes_begin = static_cast<VariableScope *>(calloc(128, sizeof(VariableScope)));
	VariableScope *scopes_current = scopes_begin - 1;
	const VariableScope *const scopes_end = scopes_begin + 128;

	StackValue *const stack_begin = static_cast<StackValue *>(calloc(128, sizeof(StackValue)));
	StackValue *stack_current = stack_begin - 1;
	const StackValue *const stack_end = stack_begin + 128;

	// print values
	auto print_indent = [](long long n) {
		for (int32_t i = 0; i < n; ++i) {
			putchar(' ');
			putchar(' ');
			putchar(' ');
			putchar(' ');
		}
	};

	while (ip < bytecode_len) {
		if (state->failed) {
			break;
		}

		uint8_t opcode = module->bytecode[ip];
		ip += 1;

		if (opcode >= OpCodeKind::Count) {
			fprintf(stderr, "Invalid opcode %u\n", opcode);
			break;
		}

		OpCodeKind opcode_kind = OpCodeKind(opcode);

		switch (opcode_kind) {
		// Stack manipulation
		case Constant: {
			int32_t value = bytecode_read_i32(module->bytecode, bytecode_len, &ip);
			print_indent(callstack_current - callstack_begin);
			printf("Pushed %d\n", value);

			EXEC_ASSERT(state, stack_current + 1 < stack_end);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);

			stack_current += 1;
			stack_current->i32 = value;
			break;
		}
		case Push: {
			EXEC_ASSERT(state, false);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			break;
		}
		// Control flow
		case Call: {
			uint32_t i_function = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			EXEC_ASSERT(state, i_function < module->functions_length);
			EXEC_ASSERT(state, callstack_current + 1 < callstack_end);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);

			Function *function = module->functions + i_function;

			print_indent(callstack_current - callstack_begin);
			printf("-> Call from %zu\n", ip);

			// Push a new call frame with the saved up return address
			callstack_current += 1;
			callstack_current->return_ip = ip;
			ip = function->address;
			break;
		}
		case Ret: {
			print_indent(callstack_current - callstack_begin);
			if (callstack_begin <= callstack_current && callstack_current < callstack_end) {
				// Pop the last callstack, restore IP
				ip = callstack_current->return_ip;
				callstack_current -= 1;
			} else {
				// There is no more callstack? Break the loop with an invalid IP value
				ip = ~uint64_t(0);
			}
			printf("<- Return to %zu\n", ip);
			break;
		}
		case ConditionalJump: {
			EXEC_ASSERT(state, stack_current >= stack_begin);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);

			auto popped = *stack_current;
			stack_current -= 1;

			print_indent(callstack_current - callstack_begin);
			printf("Popped %d\n", popped.i32);

			uint32_t jump_destination = bytecode_read_u32(module->bytecode, bytecode_len, &ip);

			if (popped.i32 != 0) {
				ip = uint64_t(jump_destination);
			}
			break;
		}
		case Jump: {
			uint32_t jump_destination = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			ip = uint64_t(jump_destination);
			break;
		}
		// Struct
		case GetField: {
			uint32_t i_field = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu): %u\n", ip, i_field);
			EXEC_ASSERT(state, false);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			break;
		}
		case SetField: {
			uint32_t i_field = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %u\n", ip, i_field);
			EXEC_ASSERT(state, false);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			break;
		}
		// Variables storage
		case BeginScope: {
			EXEC_ASSERT(state, scopes_current < scopes_end);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			scopes_current += 1;
			scopes_current->variables_length = 0;
			break;
		}
		case EndScope: {
			EXEC_ASSERT(state, scopes_current >= scopes_begin);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			scopes_current -= 1;
			break;
		}
		case SetLocal: {
			uint32_t i_local = bytecode_read_u32(module->bytecode, bytecode_len, &ip);

			EXEC_ASSERT(state, scopes_current >= scopes_begin);
			EXEC_ASSERT(state, stack_current >= stack_begin);
			EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);

			print_indent(callstack_current - callstack_begin);
			printf("Popped %d (local #%u)\n", stack_current->i32, i_local);

			scopes_current->variables[i_local] = *stack_current;
			stack_current -= 1;
			break;
		}
		case GetLocal: {
			uint32_t i_local = bytecode_read_u32(module->bytecode, bytecode_len, &ip);

			EXEC_ASSERT(state, scopes_current >= scopes_begin);
			EXEC_ASSERT(state, stack_current + 1 < stack_end);
			EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);

			stack_current += 1;
			*stack_current = scopes_current->variables[i_local];

			print_indent(callstack_current - callstack_begin);
			printf("Pushed %d (local #%u)\n", stack_current->i32, i_local);

			break;
		}
		// Maths
		case IAdd: {
			EXEC_ASSERT(state, stack_current >= stack_begin + 1);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			StackValue rhs = *stack_current;
			stack_current -= 1;
			StackValue lhs = *stack_current;
			stack_current->i32 = lhs.i32 + rhs.i32;

			print_indent(callstack_current - callstack_begin);
			printf("Popped, Popped, Pushed %d\n", stack_current->i32);

			break;
		}
		case ISub: {
			EXEC_ASSERT(state, stack_current >= stack_begin + 1);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			StackValue rhs = *stack_current;
			stack_current -= 1;
			StackValue lhs = *stack_current;
			stack_current->i32 = lhs.i32 - rhs.i32;

			print_indent(callstack_current - callstack_begin);
			printf("Popped, Popped, Pushed %d\n", stack_current->i32);

			break;
		}
		case ILessThanEq: {
			EXEC_ASSERT(state, stack_current >= stack_begin + 1);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			StackValue rhs = *stack_current;
			stack_current -= 1;
			StackValue lhs = *stack_current;
			stack_current->i32 = int32_t(lhs.i32 <= rhs.i32);

			print_indent(callstack_current - callstack_begin);
			printf("Popped, Popped, Pushed %d\n", stack_current->i32);

			break;
		}
		case Halt: {
			EXEC_ASSERT(state, false);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			state->error_msg = sv_from_null_terminated("Halt");
			break;
		}
		// Debug
		case DebugLabel: {
			sv label = bytecode_read_sv(module->bytecode, bytecode_len, &ip);
			print_indent(callstack_current - callstack_begin);
			printf("\"%.*s\"\n", int(label.length), label.chars);
			break;
		}
		case Count: {
			EXEC_ASSERT(state, false);
			EXEC_JUMP_IF_FAILED(state, EXEC_FAILED);
			break;
		}
		}
	}

	goto EXEC_SUCCESS;

EXEC_FAILED:
	EXEC_ASSERT(state, state->failed == false);
	if (state->config.error_callback != nullptr) {
		RuntimeError err = {};
		err.message = state->error_msg;
		err.file = state->error_file;
		err.line = state->error_line;
		state->config.error_callback(state, err);
	}

EXEC_SUCCESS:
	fprintf(stdout, "End of execution. Stack:\n");
	uint32_t i = 0;
	for (StackValue *s = stack_begin; s <= stack_current; ++s) {
		fprintf(stdout, "Stack(%u) = %d\n", i, s->i32);
		i += 1;
	}
}

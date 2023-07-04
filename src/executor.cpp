// TODO: Set arguments to locals when calling functions

#include "executor.h"
#include "image.h"
#include <corecrt_wstdio.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct ExecutorState
{
	Image image;
	bool failed;
	sv error_msg;
	sv error_file;
	int error_line;
	uint64_t error_ip;
};

struct CallFrame
{
	uint64_t ip;
};

union StackValue
{
	int32_t i32;
};

#define CALLSTACK_CAPACITY 8
#define STACK_CAPACITY 8
#define VARSCOPE_CAPACITY 16

struct VariableScope
{
	StackValue variables[VARSCOPE_CAPACITY];
	uint64_t variables_length;
};

ExecutorState *executor_init()
{
	ExecutorState *state = reinterpret_cast<ExecutorState *>(calloc(1, sizeof(ExecutorState)));
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

void executor_assert(ExecutorState *state, bool condition, sv condition_str, sv file, int line)
{
	if (!condition) {
		state->failed = true;
		state->error_msg = condition_str;
		state->error_file = file;
		state->error_line = line;
	}
}

#define EXEC_ASSERT(state, condition)                                                                                  \
	executor_assert(state, condition, sv_from_null_terminated(#condition), sv_from_null_terminated(__FILE__), __LINE__)

void executor_execute_module_at(ExecutorState *state, Module *module, uint64_t ip)
{
	state->failed = false;
	state->error_msg = {};
	state->error_file = {};
	state->error_line = {};
	fprintf(stdout, "\t====\n\tTRACE(%zu) Starting execution...\n", ip);

	const uint64_t bytecode_len = module->bytecode_length;
	CallFrame callstack[CALLSTACK_CAPACITY] = {};
	uint32_t callstack_length = 1;

	VariableScope scopes[8] = {};
	uint32_t scopes_length = 0;

	StackValue stack[STACK_CAPACITY] = {};
	uint32_t stack_length = 0;

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
		fprintf(stdout, "\tTRACE(%zu) Executing %s\n", ip, OpCode_str[uint8_t(opcode_kind)]);

		switch (opcode_kind) {
		// Stack manipulation
		case Constant: {
			int32_t value = bytecode_read_i32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %d\n", ip, value);

			EXEC_ASSERT(state, stack_length < STACK_CAPACITY);

			stack[stack_length].i32 = value;
			stack_length += 1;

			break;
		}
		case Push: {
			EXEC_ASSERT(state, false);
			break;
		}
		// Control flow
		case Call: {
			uint32_t i_function = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) Calling function #%u\n", ip, i_function);

			EXEC_ASSERT(state, i_function < module->functions_length);
			EXEC_ASSERT(state, callstack_length > 0);
			EXEC_ASSERT(state, callstack_length < CALLSTACK_CAPACITY);

			callstack[callstack_length - 1].ip = ip;

			callstack[callstack_length].ip = ip;
			ip = module->functions[i_function].address;

			callstack_length += 1;
			break;
		}
		case Ret: {
			if (callstack_length <= 1) {
				// break out of loop
				ip = ~uint64_t(0);
			} else {
				{
					callstack_length -= 1;
					ip = callstack[callstack_length - 1].ip;
				}
			}
			break;
		}
		case ConditionalJump: {
			EXEC_ASSERT(state, stack_length > 0);

			auto popped = stack[stack_length - 1];
			stack_length -= 1;

			fprintf(stdout, "\tTRACE(%zu) Popped %d\n", ip, popped.i32);

			uint32_t jump_destination = bytecode_read_u32(module->bytecode, bytecode_len, &ip);

			if (popped.i32 != 0) {
				fprintf(stdout, "\tTRACE(%zu) %d\n", ip, jump_destination);
				ip = uint64_t(jump_destination);
			}
			break;
		}
		case Jump: {
			uint32_t jump_destination = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %d\n", ip, jump_destination);
			ip = uint64_t(jump_destination);
			break;
		}
		// Struct
		case GetField: {
			uint32_t i_field = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu): %u\n", ip, i_field);
			EXEC_ASSERT(state, false);
			break;
		}
		case SetField: {
			uint32_t i_field = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %u\n", ip, i_field);
			EXEC_ASSERT(state, false);
			break;
		}
		// Variables storage
		case BeginScope: {
			scopes_length += 1;
			EXEC_ASSERT(state, scopes_length < 8);
			scopes[scopes_length - 1].variables_length = 0;
			break;
		}
		case EndScope: {
			EXEC_ASSERT(state, scopes_length > 0);
			scopes_length -= 1;
			break;
		}
		case SetLocal: {
			uint32_t i_local = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %u\n", ip, i_local);

			EXEC_ASSERT(state, scopes_length > 0);
			EXEC_ASSERT(state, stack_length > 0);
			EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);

			fprintf(stdout, "\tTRACE(%zu) Set Local #%u = %d\n", ip, i_local, stack[stack_length - 1].i32);

			scopes[scopes_length - 1].variables[i_local] = stack[stack_length - 1];
			stack_length -= 1;

			break;
		}
		case GetLocal: {
			uint32_t i_local = bytecode_read_u32(module->bytecode, bytecode_len, &ip);

			EXEC_ASSERT(state, scopes_length > 0);
			EXEC_ASSERT(state, stack_length < STACK_CAPACITY);
			EXEC_ASSERT(state, i_local < VARSCOPE_CAPACITY);

			stack[stack_length] = scopes[scopes_length - 1].variables[i_local];

			fprintf(stdout, "\tTRACE(%zu) Get Local #%u = %d\n", ip, i_local, stack[stack_length].i32);

			stack_length += 1;

			break;
		}
		// Maths
		case IAdd: {
			EXEC_ASSERT(state, stack_length >= 2);
			StackValue rhs = stack[stack_length - 1];
			StackValue lhs = stack[stack_length - 2];
			stack[stack_length - 2].i32 = lhs.i32 + rhs.i32;
			stack_length -= 1;
			break;
		}
		case ISub: {
			EXEC_ASSERT(state, stack_length >= 2);
			StackValue rhs = stack[stack_length - 1];
			StackValue lhs = stack[stack_length - 2];
			stack[stack_length - 2].i32 = lhs.i32 - rhs.i32;
			stack_length -= 1;
			break;
		}
		case ILessThanEq: {
			EXEC_ASSERT(state, stack_length >= 2);
			StackValue rhs = stack[stack_length - 1];
			StackValue lhs = stack[stack_length - 2];
			stack[stack_length - 2].i32 = int32_t(lhs.i32 <= rhs.i32);
			stack_length -= 1;
			fprintf(stdout, "\tTRACE(%zu) Top of stack: %d\n", ip, stack[stack_length - 1].i32);
			break;
		}
		case Halt: {
			EXEC_ASSERT(state, false);
			state->error_msg = sv_from_null_terminated("Halt");
			break;
		}
		// Debug
		case DebugLabel: {
			sv label = bytecode_read_sv(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %.*s\n", ip, int(label.length), label.chars);
			break;
		}
		case Count: {
			EXEC_ASSERT(state, false);
			break;
		}
		}
	}

	if (state->failed) {
		if (state->error_file.chars != nullptr) {
			fprintf(stderr, "%.*s:%d  ", int(state->error_file.length), state->error_file.chars, state->error_line);
		}
		fprintf(stderr, "EXECUTOR FAILED: %.*s\n", int(state->error_msg.length), state->error_msg.chars);
	}

	fprintf(stdout, "\tTRACE End of execution. Stack:\n");
	for (uint32_t i = 0; i < stack_length; ++i) {
		fprintf(stdout, "\tStack(%u) = %d\n", i, stack[i].i32);
	}
}

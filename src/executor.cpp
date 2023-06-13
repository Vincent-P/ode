#include "executor.h"
#include "image.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct ExecutorState
{
	Image image;
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

void executor_execute_module_at(ExecutorState *state, Module *module, uint64_t ip)
{
	(void)(state);

	const uint64_t bytecode_len = module->bytecode_length;

	while (ip < bytecode_len) {
		uint8_t opcode = module->bytecode[ip];
		ip += 1;

		if (opcode >= OpCodeKind::Count) {
			fprintf(stderr, "Invalid opcode %u\n", opcode);
			break;
		}

		OpCodeKind opcode_kind = OpCodeKind(opcode);
		fprintf(stdout, "\tTRACE(%zu) Executing %s\n", ip, OpCode_str[uint8_t(opcode_kind)]);

		switch (opcode) {
		// Stack manipulation
		case Constant: {
			int32_t value = bytecode_read_i32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %d\n", ip, value);
			break;
		}
		case Push: {
			break;
		}
		// Control flow
		case Call: {
			break;
		}
		case Ret: {
			break;
		}
		// Struct
		case GetField: {
			uint32_t i_field = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu): %u\n", ip, i_field);
			break;
		}
		case SetField: {
			uint32_t i_field = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %u\n", ip, i_field);
			break;
		}
		// Variables storage
		case BeginScope: {
			break;
		}
		case EndScope: {
			break;
		}
		case SetLocal: {
			uint32_t i_local = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %u\n", ip, i_local);
			break;
		}
		case GetLocal: {
			uint32_t i_local = bytecode_read_u32(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %u\n", ip, i_local);
			break;
		}
		// Maths
		case IAdd: {
			break;
		}
		case ILessThanEq: {
			break;
		}
		case Halt: {
			break;
		}
		// Debug
		case DebugLabel: {
			sv label = bytecode_read_sv(module->bytecode, bytecode_len, &ip);
			fprintf(stdout, "\tTRACE(%zu) %.*s\n", ip, int(label.length), label.chars);
			break;
		}
		case Count: {
			break;
		}
		}
	}
}

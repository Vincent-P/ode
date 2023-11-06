#pragma once
#include <stdint.h>

const uint32_t BYTECODE_LENGTH = (32 << 10);

union Value
{
	int32_t  i32; // int
	float    f32; // float
	uint32_t u32; // uint or ptr or bool
};

enum struct ValueKind
{
	I32,
	F32,
	U32,
	PTR,
	BOOL,
};

struct Module
{
	uint32_t function_addresses[64]; // jump table
	uint8_t bytecode[BYTECODE_LENGTH];
	uint32_t bytecode_len;
};

struct ExecutionContext
{
	// code
	Module modules[64];
	uint32_t modules_len;
	// operand stack
	Value stack[64];
	// callframe stack
	uint32_t callstack_ret_module[64];
	uint32_t callstack_ret_address[64];
	uint32_t callstack_ret_bp[64];
	uint32_t callstack_argc[64]; // number of arguments
};

void call_function(ExecutionContext *ctx, uint32_t callee_module, uint32_t callee_function, Value *args, uint32_t args_len);

#pragma once
#include <stdint.h>
#include "core.h"

enum Executor_Constants
{
	BYTECODE_LENGTH = (32 << 10),
	IMPORT_LENGTH = 64,
};

typedef union Value
{
	int32_t  i32; // int
	float    f32; // float
	uint32_t u32; // uint or ptr or bool
} Value;

typedef enum ValueKind
{
	ValueKind_I32,
	ValueKind_F32,
	ValueKind_U32,
	ValueKind_PTR,
	ValueKind_BOOL,
} ValueKind;

typedef void(*ForeignFn)(void);
typedef struct Module
{
	sv name;
	sv import_module_names[IMPORT_LENGTH]; // module name
	sv import_names[IMPORT_LENGTH]; // function name
	uint32_t import_module[IMPORT_LENGTH]; // runtime module index
	uint64_t import_addresses[IMPORT_LENGTH]; // function address in module
	uint32_t import_length;
	
	sv export_names[IMPORT_LENGTH]; // function name
	uint32_t export_addresses[IMPORT_LENGTH]; // function address
	uint32_t export_length;

	sv foreign_function_module_names[IMPORT_LENGTH];
	sv foreign_function_names[IMPORT_LENGTH];
	ForeignFn foreign_function_callback[IMPORT_LENGTH];
	uint32_t foreign_function_length;

	uint8_t bytecode[BYTECODE_LENGTH];
	uint32_t bytecode_len;
} Module;

// We will need to move modules out of the context to support multiple threads of execution
typedef struct ExecutionContext
{
	// code
	Module *modules;
	uint32_t modules_len;
	// operand stack
	Value stack[64];
	// callframe stack
	uint32_t callstack_ret_module[64];
	uint32_t callstack_ret_address[64];
	uint32_t callstack_ret_bp[64];
	uint32_t callstack_argc[64]; // number of arguments
} ExecutionContext;

void call_function(ExecutionContext *ctx, uint32_t callee_module, uint32_t callee_ip, Value *args, uint32_t args_len);

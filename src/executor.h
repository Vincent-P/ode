#pragma once
#include <stdint.h>
#include "core.h"

enum Executor_Constants
{
	CONSTANT_LENGTH = 128,
	BYTECODE_LENGTH = (32 << 10),
	IMPORT_LENGTH = 64,
};

typedef enum PointerType
{
	PointerType_Host,
	PointerType_Image,
	PointerType_Heap,
} PointerType;
enum PointerTypeCount {
	PointerType_Count = PointerType_Heap + 1,
};

typedef struct Pointer
{
	uint32_t offset : 30;
	PointerType type : 2;
} Pointer;
_Static_assert(sizeof(Pointer) == sizeof(uint32_t));

typedef struct Slice
{
	Pointer ptr;
	uint32_t length;
} Slice;
_Static_assert(sizeof(Slice) == sizeof(uint64_t));

typedef union Value
{
	uint8_t u8;
	uint16_t u16;
	uint32_t u32;
	uint32_t u64;
	int8_t i8;
	int16_t i16;
	int32_t  i32;
	int64_t i64;
	float    f32;
	Pointer ptr;
	Slice slice;
} Value;
_Static_assert(sizeof(Value) == sizeof(uint64_t));

typedef void(*ForeignFn)(Value*, uint32_t);

typedef struct Module
{
	sv name;
	// Imported functions
	sv import_module_names[IMPORT_LENGTH]; // module name
	sv import_names[IMPORT_LENGTH]; // function name
	uint32_t import_module[IMPORT_LENGTH]; // runtime module index
	uint64_t import_addresses[IMPORT_LENGTH]; // function address in module
	uint32_t import_length;
	// Exported functions
	sv export_names[IMPORT_LENGTH]; // function name
	uint32_t export_addresses[IMPORT_LENGTH]; // function address
	uint32_t export_length;
	// Foreign functions
	sv foreign_function_module_names[IMPORT_LENGTH];
	sv foreign_function_names[IMPORT_LENGTH];
	ForeignFn foreign_function_callback[IMPORT_LENGTH];
	uint32_t foreign_function_length;
	// Constant memory
	uint8_t constants[CONSTANT_LENGTH];
	// Bytecode
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

#pragma once
#include <stdint.h>
#include "core/core.h"

typedef struct ExecutionContext ExecutionContext;

enum Executor_Constants
{
	CONSTANT_LENGTH = 128,
	IMPORT_LENGTH = 64,
};

typedef enum PointerType
{
	PointerType_Host = 0,
	PointerType_Image = 1,
	PointerType_Heap = 2,
} PointerType;
enum PointerTypeCount {
	PointerType_Count = PointerType_Heap + 1,
};

typedef struct Pointer
{
	uint32_t offset : 22;
	uint32_t module : 7;
	PointerType type : 3;
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
	uint64_t u64;
	int8_t i8;
	int16_t i16;
	int32_t  i32;
	int64_t i64;
	float    f32;
	Pointer ptr;
	Slice slice;
} Value;
_Static_assert(sizeof(Value) == sizeof(uint64_t));

typedef void(*ForeignFn)(ExecutionContext*,Value*, uint32_t, uint32_t*);

typedef struct Module
{
	StringId name;
	// Imported functions
	StringId import_module_names[IMPORT_LENGTH]; // module name
	StringId import_names[IMPORT_LENGTH]; // function name
	uint32_t import_module[IMPORT_LENGTH]; // runtime module index
	uint64_t import_addresses[IMPORT_LENGTH]; // function address in module
	uint32_t import_length;
	// Exported functions
	StringId export_names[IMPORT_LENGTH]; // function name
	uint32_t export_addresses[IMPORT_LENGTH]; // function address
	uint32_t export_length;
	// Foreign functions
	StringId foreign_function_module_names[IMPORT_LENGTH];
	StringId foreign_function_names[IMPORT_LENGTH];
	ForeignFn foreign_function_callback[IMPORT_LENGTH];
	uint32_t foreign_function_length;
	// Constant memory
	uint8_t constants[CONSTANT_LENGTH];
	// Bytecode
	uint8_t bytecode[BYTECODE_CAPACITY];
	uint32_t bytecode_len;
} Module;

// Ideally it should be completely opaque and hidden in executor.c
typedef struct ExecutionContext
{
	// code
	const Module *modules;
	uint32_t modules_len;
	// operand stack
	Value stack[64];
	// callframe stack
	uint32_t callstack_ret_module[64];
	uint32_t callstack_ret_address[64];
	uint32_t callstack_ret_bp[64];
	uint32_t callstack_argc[64]; // number of arguments
	// memory
	uint8_t *heap;
} ExecutionContext;

void call_function(ExecutionContext *ctx, uint32_t callee_module, uint32_t callee_ip, Value *args, uint32_t args_len);
const uint8_t *deref_pointer(ExecutionContext *ctx, Pointer ptr);
uint8_t *deref_pointer_mut(ExecutionContext *ctx, Pointer ptr);

// host
Value make_heap_pointer(ExecutionContext *ctx, uint32_t offset_in_heap);

#pragma once
#include "core/core.h"
#include "error.h"
#include "compiler.h"
#include "executor.h"

typedef struct VM VM;
typedef struct CompilerModule CompilerModule;
typedef struct Module Module;
typedef struct Arena Arena;

typedef struct VMConfig
{
	bool (*load_module)(sv module_name, sv *out_code);
	void (*on_module_compiled)(sv module_name);
	void (*error_callback)(VM *, Error);
	ForeignFn (*foreign_callback)(sv module_name, sv function_name);
	uint8_t *heap;
} VMConfig;

typedef struct VM
{
	Arena memory;
	VMConfig config;
	CompilerModule compiler_modules[8];
	Module runtime_modules[8];
	uint32_t compiler_modules_length;
	uint32_t runtime_modules_length;

	StringPool identifiers_pool;
} VM;

VM *vm_create(Arena *arena, VMConfig config);
Error vm_compile(Arena temp_mem, VM* vm, sv module_name, sv code);
// compile and try to execute "main" function
Error vm_call(VM *vm, sv module_name, sv function_name, Arena temp_mem);
void vm_destroy(VM *vm);

#pragma once
#include "core.h"
#include "arena.h"
#include "error.h"
#include "compiler.h"
#include "executor.h"

struct VM;
struct CompilerModule;
struct Module;
struct Arena;

struct VMConfig
{
	bool (*load_module)(sv module_name, sv *out_code);
	void (*error_callback)(VM *, Error);
	ForeignFn (*foreign_callback)(sv module_name, sv function_name);
};

struct VM
{
	VMConfig config;
	CompilerModule compiler_modules[8];
	uint32_t compiler_modules_length;
	Module runtime_modules[8];
	uint32_t runtime_modules_length;
};

VM *vm_create(Arena *arena, VMConfig config);
Error vm_compile(VM* vm, sv module_name, sv code);
// compile and try to execute "main" function
void vm_call(VM *vm, sv module_name, sv function_name, Arena temp_mem);
void vm_destroy(VM *vm);

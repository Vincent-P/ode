#pragma once
#include "core.h"
#include "error.h"

struct VM;
struct CompilerModule;
struct Module;

using ForeignFn = void (*)(VM *);

struct VMConfig
{
	bool (*load_module)(sv module_name, sv *out_code);
	void (*error_callback)(VM *, Error);
	ForeignFn (*foreign_callback)(sv module_name, sv function_name);
};

struct VM
{
	VMConfig config;
	vec<CompilerModule> compiler_modules;
	vec<Module> runtime_modules;
};

VM *vm_create(VMConfig config);
Error vm_compile(VM* vm, sv module_name, sv code);
// compile and try to execute "main" function
void vm_call(VM* vm, sv module_name, sv function_name);
void vm_destroy(VM *vm);

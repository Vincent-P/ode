#pragma once
#include "core.h"
#include "error.h"

struct VM;

using ForeignFn = void (*)(VM *);

struct VMConfig
{
	bool (*load_module)(sv module_name, sv *out_code);
	void (*error_callback)(VM *, Error);
	ForeignFn (*foreign_callback)(sv module_name, sv function_name);
};

struct FunctionInstance
{
	ForeignFn foreign;
};

struct ModuleInstance
{
	FunctionInstance *functions;
	uint64_t functions_capacity;
	uint64_t functions_length;
};

struct VM
{
	VMConfig config;
	vec<Module> modules;
	vec<ModuleInstance> module_instances;
};

VM *vm_create(VMConfig config);
void vm_compile(VM* vm, sv module_name, sv code);
// compile and try to execute "main" function
void vm_interpret(VM* vm, sv module_name, sv code);
void vm_destroy(VM *vm);

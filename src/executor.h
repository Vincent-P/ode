#pragma once
#include "core.h"

struct RuntimeError;
struct Image;
struct ExecutorState;
struct Module;

using ForeignFn = void (*)();

struct ExecutorConfig
{
	void (*error_callback)(ExecutorState *, RuntimeError);
	ForeignFn (*foreign_callback)(sv module_name, sv function_name);
};

ExecutorState *executor_init(ExecutorConfig config);
void executor_load_module(ExecutorState *state, Module *module);
void executor_execute_module_entrypoint(ExecutorState *state, sv module_name);

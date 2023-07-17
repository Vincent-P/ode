#pragma once
#include "core.h"

struct RuntimeError;
struct Image;
struct ExecutorState;
struct Module;
struct ExecutionContext;

using ForeignFn = void (*)(ExecutionContext *);

struct ExecutorConfig
{
	void (*error_callback)(ExecutorState *, RuntimeError);
	ForeignFn (*foreign_callback)(sv module_name, sv function_name);
};

ExecutorState *executor_init(ExecutorConfig config);
void executor_load_module(ExecutorState *state, Module *module);
void executor_execute_module_entrypoint(ExecutorState *state, sv module_name);

// Operand stack
union StackValue
{
	bool b8;
	float f32;
	int32_t i32;
	uint8_t *local_storage_offset;
};

StackValue execution_get_local(ExecutionContext *ctx, uint32_t i_local);

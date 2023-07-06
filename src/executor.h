#pragma once
#include "core.h"

struct RuntimeError;
struct Image;
struct ExecutorState;

struct ExecutorConfig
{
	void (*error_callback)(ExecutorState *, RuntimeError);
};

ExecutorState *executor_init(ExecutorConfig config);
void executor_load_image(ExecutorState *state, Image *image);
void executor_execute_module_entrypoint(ExecutorState *state, sv module_name);

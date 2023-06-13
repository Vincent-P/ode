#pragma once
#include "core.h"

struct Image;
struct ExecutorState;
ExecutorState *executor_init();
void executor_load_image(ExecutorState *state, Image *image);
void executor_execute_module_entrypoint(ExecutorState *state, sv module_name);

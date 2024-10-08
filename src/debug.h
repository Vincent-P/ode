#pragma once
#include "core/core.h"

void print_bytecode(const uint8_t *bytecode, uint32_t bytecode_length);
void build_error_at(sv code, span error, StringBuilder *sb);

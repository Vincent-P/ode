#pragma once
#include "core.h"

struct Token;
struct AstNode;
void print_ast(sv input, slice<const Token> tokens, slice<const AstNode> nodes, uint32_t root_index);
void print_bytecode(const uint8_t *bytecode, uint32_t bytecode_length);

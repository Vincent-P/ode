#pragma once
#include "core.h"
#include "image.h"
#include "error.h"

struct CompilationUnit
{
	sv input;
	vec<AstNode> nodes;
	vec<Token> tokens;
	Error error;
};

struct Compiler;
Compiler *compiler_init();
Error compile_module(Compiler *compiler, sv module_name, sv input, Module **out_module);

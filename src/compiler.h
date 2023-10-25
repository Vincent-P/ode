#pragma once
#include "core.h"
#include "module.h"
#include "error.h"

struct VM;

struct CompilationUnit
{
	sv input;
	vec<AstNode> nodes;
	vec<Token> tokens;
	Error error;
};

struct LexicalScope
{
	sv *variables_name;
	TypeID *variables_type;
	uint32_t variables_length;
};

struct Compiler
{
	VM *vm;
	CompilationUnit *compunit;
	
	vec<LexicalScope> scopes;
	Module module;
};

void compiler_scan_requires(Compiler *compiler, const Token** out_tokens, uint32_t out_tokens_max_length, uint32_t *out_tokens_written);
void compile_module(Compiler *compiler);



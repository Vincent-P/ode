#pragma once
#include "core.h"
#include "constants.h"
#include "error.h"
#include "parser.h"

typedef struct VM VM;

typedef struct CompilationUnit
{
	sv input;
	Error error;
	uint32_t nodes_length;
	uint32_t tokens_length;
	AstNode nodes[4096];
	Token tokens[4096];
} CompilationUnit;

typedef struct LexicalScope
{
	sv args_name[SCOPE_MAX_VARIABLES];
	TypeID args_type[SCOPE_MAX_VARIABLES];
	uint32_t args_length;
	
	sv variables_name[SCOPE_MAX_VARIABLES];
	TypeID variables_type[SCOPE_MAX_VARIABLES];
	uint32_t variables_length;
} LexicalScope;

typedef struct UserDefinedType
{
	sv name;
	sv field_names[MAX_STRUCT_FIELDS];
	TypeID field_types[MAX_STRUCT_FIELDS];
	uint32_t field_offsets[MAX_STRUCT_FIELDS];
	uint32_t field_count;
	uint32_t size;
} UserDefinedType;

typedef enum FunctionType
{
	FunctionType_Local,
	FunctionType_Global,
	FunctionType_Foreign
} FunctionType;

typedef struct Function
{
	sv name;
	uint32_t address; // offset into the compiler bytecode
	TypeID arg_types[MAX_ARGUMENTS];
	uint32_t arg_count;
	TypeID return_type;
	FunctionType type;
} Function;

typedef struct CompilerModule
{
	sv name;
	// functions containes in this module
	Function functions[16];
	uint32_t functions_length;
	// types defined in this  i32;module
	UserDefinedType types[8];
	uint32_t types_length;
	// table of imports module index
	uint32_t *imports;
	uint32_t imports_length;
	// imported functions
	uint32_t imported_module_indices[8];
	uint32_t imported_function_indices[8];
	uint32_t imported_functions_length;
	// foreign functions
	sv foreign_functions_module_name[8];
	sv foreign_functions_name[8];
	uint32_t foreign_functions_length;
	// bytecode for all the functions
	uint8_t bytecode[1024];
	uint32_t bytecode_length;
} CompilerModule;

typedef struct Compiler
{
	VM *vm;
	CompilationUnit *compunit;
	
	LexicalScope scopes[16];
	uint32_t scopes_length;
	CompilerModule module;
} Compiler;
	
void compiler_scan_requires(CompilationUnit *compunit, const Token **out_tokens, uint32_t out_tokens_max_length, uint32_t *out_tokens_written);
void compile_module(Compiler *compiler);



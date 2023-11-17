#pragma once
#include "core.h"
#include "constants.h"
#include "error.h"

struct VM;
struct AstNode;

struct CompilationUnit
{
	sv input;
	vec<AstNode> nodes;
	vec<Token> tokens;
	Error error;
};

struct LexicalScope
{
	uint32_t i_outer_function;
	sv *args_name;
	TypeID *args_type;
	uint32_t args_length;
	sv *variables_name;
	TypeID *variables_type;
	uint32_t variables_length;
};

struct UserDefinedType
{
	sv name;
	sv field_names[MAX_STRUCT_FIELDS];
	TypeID field_types[MAX_STRUCT_FIELDS];
	uint32_t field_offsets[MAX_STRUCT_FIELDS];
	uint32_t field_count;
	uint32_t size;
};

enum struct FunctionType : uint8_t
{
	Local,
	Global,
	Foreign
};

struct Function
{
	sv name;
	uint32_t address; // offset into the compiler bytecode
	TypeID arg_types[MAX_ARGUMENTS];
	uint32_t arg_count;
	TypeID return_type;
	FunctionType type;
};

struct CompilerModule
{
	sv name;
	// functions containes in this module
	Function *functions;
	uint32_t functions_capacity;
	uint32_t functions_length;
	// bytecode for all the functions
	uint8_t *bytecode;
	uint32_t bytecode_capacity;
	uint32_t bytecode_length;
	// types defined in this  i32;module
	UserDefinedType *types;
	uint32_t types_capacity;
	uint32_t types_length;
	// table of imports module index
	uint32_t *imports;
	uint32_t imports_length;
	// imported functions
	uint32_t *imported_module_indices;
	uint32_t *imported_function_indices;
	uint32_t imported_functions_length;
};


struct Compiler
{
	VM *vm;
	CompilationUnit *compunit;
	
	vec<LexicalScope> scopes;
	CompilerModule module;
};
	
void compiler_scan_requires(CompilationUnit *compunit, const Token **out_tokens, uint32_t out_tokens_max_length, uint32_t *out_tokens_written);
void compile_module(Compiler *compiler);



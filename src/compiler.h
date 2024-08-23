#pragma once
#include "core/core.h"
#include "constants.h"
#include "error.h"
#include "parser.h"

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
	StringId name;
	uint32_t address; // offset into the compiler bytecode, OR foreign function index
	TypeID arg_types[MAX_ARGUMENTS];
	uint32_t arg_count;
	TypeID return_type;
	FunctionType type;
} Function;

typedef struct CompilerModule
{
	StringId name;
	// functions contained in this module (native, global, or foreign)
	Function functions[16];
	uint32_t functions_length;
	// types defined in this module
	UserDefinedType types[8];
	uint32_t types_length;
	// table of imports module index
	uint32_t *imports;
	uint32_t imports_length;
	// imported functions
	uint32_t imported_module_indices[8];
	uint32_t imported_function_indices[8];
	uint32_t imported_functions_length;
	// foreign functions, either from this module, or imported from other module
	StringId foreign_functions_module_name[8];
	StringId foreign_functions_name[8];
	uint32_t foreign_functions_length;
	// constant memory
	uint8_t constants[128];
	uint32_t constants_length;
	// bytecode for all the functions
	uint8_t bytecode[BYTECODE_CAPACITY];
	uint32_t bytecode_length;
} CompilerModule;
	
typedef struct ScanDepsResult
{
	Error error;
	StringId *names;
	uint32_t count;
	bool success;
} ScanDepsResult;

ScanDepsResult compiler_scan_dependencies(Arena *memory, StringPool *token_strings, Token const *tokens, AstNode const *nodes);


typedef struct CompilerInput
{
	StringPool *string_pool;
	const Token *tokens;
	const AstNode *nodes;
} CompilerInput;

struct CompileModuleResult
{

};
typedef struct CompileModuleResult CompileModuleResult;
CompileModuleResult compile_module(CompilerInput input);



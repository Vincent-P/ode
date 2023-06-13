#pragma once
#include "core.h"

inline constexpr uint64_t MAX_ARGUMENTS = 8;

enum OpCodeKind : uint8_t
{
	// Stack manipulation
	Constant,
	Push,
	// Control flow
	Call,
	Ret,
	// Struct
	GetField,
	SetField,
	// Variables storage
	BeginScope,
	EndScope,
	SetLocal,
	GetLocal,
	// Maths
	IAdd,
	ILessThanEq,
	Halt,
	// Debug
	DebugLabel,
	Count,
};
inline const char *OpCode_str[] = {
	"Constant",
	"Push",
	"Call",
	"Ret",
	"GetField",
	"SetField",
	"BeginScope",
	"EndScope",
	"SetLocal",
	"GetLocal",
	"IAdd",
	"ILessThanEq",
	"Halt",
	"DebugLabel",
};
static_assert(ARRAY_LENGTH(OpCode_str) == uint8_t(OpCodeKind::Count));

enum struct TypeKind : uint8_t
{
	Int,
	Bool,
	Float,
	Pointer,
	Struct,
	Count,
};
inline constexpr const char *TypeKind_str[] = {
	"i32",
	"bool",
	"f32",
	"ptr",
	"struct",
};
static_assert(ARRAY_LENGTH(TypeKind_str) == uint64_t(TypeKind::Count));

inline constexpr uint64_t TypeKind_size[] = {
	4,
	1,
	4,
	4,
	1,
};
static_assert(ARRAY_LENGTH(TypeKind_size) == uint64_t(TypeKind::Count));

struct Type;
struct StructType
{
	sv name;
	sv field_names[8];
	Type *field_types[8];
	uint64_t field_offsets[8];
	uint64_t field_count;
};

struct Type
{
	TypeKind kind;
	uint64_t size;
	union
	{
		Type *pointee;
		StructType structure;
	} data;
};

struct Function
{
	sv name;
	uint64_t address; // offset into the compiler bytecode
	Type *arg_types[MAX_ARGUMENTS];
	uint64_t return_type;
};

struct Module
{
	sv name;
	Function *functions;
	uint64_t functions_capacity;
	uint64_t functions_length;
	uint8_t *bytecode;
	uint64_t bytecode_capacity;
	uint64_t bytecode_length;
	// Permanent types (declared structs)
	Type *types;
	uint64_t types_capacity;
	uint64_t types_length;
};

struct Image
{
	Module *modules;
	uint64_t modules_length;
	uint64_t modules_capacity;
};

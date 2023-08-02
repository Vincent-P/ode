#pragma once
#include "core.h"

inline constexpr uint64_t MAX_ARGUMENTS = 8;

enum OpCodeKind : uint8_t
{
	// Operand stack manipulation
	Constant,
	// Control flow
	Call,
	CallForeign,
	Ret,
	ConditionalJump,
	Jump,
	// Struct
	GetField,
	SetField,
	// Local storage
	BeginScope,
	EndScope,
	SetLocal,
	GetLocal,
	MakeStruct,
	// Maths
	IAdd,
	ISub,
	ILessThanEq,
	Halt,
	// Debug
	DebugLabel,
	Count,
};
inline const char *OpCode_str[] = {
	"Constant",
	"Call",
	"CallForeign",
	"Ret",
	"ConditionalJump",
	"Jump",
	"GetField",
	"SetField",
	"BeginScope",
	"EndScope",
	"SetLocal",
	"GetLocal",
	"MakeStruct",
	"IAdd",
	"ISub",
	"ILessThanEq",
	"Halt",
	"DebugLabel",
};
static_assert(ARRAY_LENGTH(OpCode_str) == uint8_t(OpCodeKind::Count));
// Has to be uint32_t for bitfields
enum struct BuiltinTypeKind : uint32_t
{
	Unit,
	Int,
	Bool,
	Float,
	Pointer,
	Count,
};
static_assert(uint8_t(BuiltinTypeKind::Count) < 16u, "Should fit on 4 bits.");

inline constexpr const char *BuiltinTypeKind_str[] = {
	"()",
	"i32",
	"bool",
	"f32",
	"ptr",
};
static_assert(ARRAY_LENGTH(BuiltinTypeKind_str) == uint64_t(BuiltinTypeKind::Count));

inline constexpr uint32_t BuiltinTypeKind_size[] = {
	0,
	4,
	1,
	4,
	4,
};
static_assert(ARRAY_LENGTH(BuiltinTypeKind_size) == uint64_t(BuiltinTypeKind::Count));

inline constexpr uint32_t TYPE_MAX_INDIRECTION = 4;
inline constexpr uint32_t TYPE_MAX_USER_DEFINED = (1 << 19); // match the user_defined_index
struct PointerTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind builtin_kind : 4;
	BuiltinTypeKind pointee_builtin_kind : 4;
	uint32_t indirection_count : 4;
	uint32_t user_defined_index : 19;
};
static_assert(sizeof(PointerTypeID) == sizeof(uint32_t), "is a uint32_t");

struct BuiltinTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind kind : 4;
	uint32_t padding : 27;
};
static_assert(sizeof(BuiltinTypeID) == sizeof(uint32_t), "is a uint32_t");

struct UserDefinedTypeID
{
	uint32_t is_user_defiend : 1;
	uint32_t index : 31;
};

union TypeID
{
	PointerTypeID pointer;
	BuiltinTypeID builtin;
	UserDefinedTypeID user_defined;
	uint32_t raw;
};
static_assert(sizeof(TypeID) == sizeof(uint32_t), "is a uint32_t");
inline bool type_id_is_pointer(TypeID id)
{
	return id.builtin.is_user_defined == 0 && id.builtin.kind == BuiltinTypeKind::Pointer;
}
inline bool type_id_is_builtin(TypeID id)
{
	return id.builtin.is_user_defined == 0 && id.builtin.kind != BuiltinTypeKind::Pointer;
}
inline bool type_id_is_user_defined(TypeID id)
{
	return id.builtin.is_user_defined == 1;
}
constexpr TypeID type_id_new_builtin(BuiltinTypeKind kind)
{
	TypeID id = {};
	id.builtin.is_user_defined = 0;
	id.builtin.kind = kind;
	id.builtin.padding = 0;
	return id;
}

inline TypeID type_id_new_user_defined(uint32_t index)
{
	TypeID id = {};
	id.user_defined.is_user_defiend = 1;
	id.user_defined.index = index;
	return id;
}

constexpr TypeID UNIT_TYPE = type_id_new_builtin(BuiltinTypeKind::Unit);

// A user-defined type, only struct for now
inline constexpr uint32_t MAX_STRUCT_FIELD = 8;
struct UserDefinedType
{
	sv name;
	sv field_names[MAX_STRUCT_FIELD];
	TypeID field_types[MAX_STRUCT_FIELD];
	uint32_t field_offsets[MAX_STRUCT_FIELD];
	uint32_t field_count;
	uint32_t size;
};

struct Function
{
	sv name;
	uint64_t address; // offset into the compiler bytecode
	TypeID arg_types[MAX_ARGUMENTS];
	uint64_t arg_count;
	TypeID return_type;
	bool is_foreign;
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
	UserDefinedType *types;
	uint32_t types_capacity;
	uint32_t types_length;
};

struct RuntimeError
{
	sv message;
	sv file;
	int line;
};

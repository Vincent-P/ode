#pragma once
#include "core.h"

inline constexpr uint64_t MAX_ARGUMENTS = 8;

// TODO: How to distinguish between GetField (which returns the field by value) and "taking the address of a field"
// - Make GetField returns an lvale and shit happens
// - Make a field-addr instead? Passing struct as parameters should be formalized! Should we pass implicitly by ptr? Should we always pass (addr my-struct-value) to functions??
// Should we disable passing structs entirely?? (and make a special case to pass "small" structs by value in the operand stack. So in the compiler we only allow struct of GIVEN size of max
// If we want enums, we will have "small" structs!

// TODO: Make function arguments read-only = Not addressable!!
// TODO: Implement small struct inlining on the stack
// TODO: Now we can only pass structs as arguments when they are small OR by pointer
// TOOD: GetField should probably also works like this? or returns a ptr
//       If not returning a ptr, add a field-addr             easy: (field-addr (addr p) x), nested structs: (field-addr (addr mat) row1 x), pointer field:(field-addr (field-addr (addr ext-mat) row-ptr) x)
//       With such impl, get field just becomes a dereference of field-addr: (get-field p x)   <=> (read-i32 (field-addr p x))    <=> (memread (field-addr p x) (sizeof i32))
//       Ditto for SetField                                                  (set-field p x 1) <=> (write-i32 (field-addr p x) 1) <=> (memset (field-addr p x) 1 (sizeof i32))


// Maybe local struct objects should be convertible to ptr (because that's how they are passed to functions in StackValues)
// But it is a bit weird, i have to figure out how to pass members also.

// Opcode(<bytecode params>) ^_pop_^ v_push_v
enum struct OpCodeKind : uint8_t
{
	// Operand stack manipulation
	Constant,
	ConstantStr,
	// Control flow
	Call,
	CallForeign,
	Ret,
	ConditionalJump,
	Jump,
	// Struct
	GetField,  // GetField(i_field) ^_StructPtr_^ v_FieldValue_v
	SetField,  // SetField(i_field) ^_FieldValue_^ ^_StructPtr_^
	// Memory
	// Local storage
	BeginScope,
	EndScope,
	SetLocal,   // SetLocal(i_local) ^_Value_^ 
	GetLocal,   // GetLocal(i_local) v_Local_v
	MakeStruct, // MakeStruct(TypeID) v_StructPtr_v
	// Pointers
	Addr,       // Addr(TypeID) ^_Value_^ v_ValuePtr_v
	Deref,      // Deref ^_ValuePtr_^ v_ValueAsObject_v 
	WriteI32,   // WriteI32 ^_Value_^ ^_PointerToWrite_^
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
	"ConstantStr",
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
	"Addr",
	"Deref",
	"WriteI32",
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
	Str,
	Count,
};
static_assert(uint8_t(BuiltinTypeKind::Count) < 16u, "Should fit on 4 bits.");

inline constexpr const char *BuiltinTypeKind_str[] = {
	"()",
	"i32",
	"bool",
	"f32",
	"ptr",
	"str",
};
static_assert(ARRAY_LENGTH(BuiltinTypeKind_str) == uint64_t(BuiltinTypeKind::Count));

inline constexpr uint32_t BuiltinTypeKind_size[] = {
	0,
	4,
	1,
	4,
	4,
	8,
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

struct Constant
{
	sv str;
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
	Constant *constants;
	uint32_t constants_capacity;
	uint32_t constants_length;
};

struct RuntimeError
{
	sv message;
	sv file;
	int line;
};

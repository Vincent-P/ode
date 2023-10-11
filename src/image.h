#pragma once
#include "core.h"


struct Module;

inline constexpr uint32_t MAX_ARGUMENTS = 8;

/**
   The VM operates with 2 memories: the operand stack, and the memory stack.
   The operand stack is a stack of registers, containing operand for the different opcodes.
   The memory stack contains addressable memory, and values that cannot be inlined in the operand stack (when they are
too big to fit, or because they need to be addressable or mutable).
**/

enum struct OpCodeKind : uint8_t
{
	// Operand stack
	LoadConstantU32, // Push a uint32_t
	LoadConstantStr, // Push the index of a str constant
	                 // Control flow
	Call,        // Read the immediate index of the function to jump to, and creates a callstack
	CallForeign, // Call the foreign callback of the current function (TODO: pop index and get args through builtins)
	Ret,         // Jump to the previous callstack
	ConditionalJump, // Pop a predicate, jump to immediate address if true
	Jump,            // Jump to immediate address
	Halt,            // Stop execution
	      // Memory stack
	StoreLocal, // SetLocal(i_local) ^_Value_^
	LoadLocal,  // GetLocal(i_local) v_Local_v
	StackAlloc,
	           // Pointers
	Load,     // Read immediaste TypeID, Pop a pointer and push the pointed value
	Store,    // Read immediate TypeID Pop a pointer, Pop value, set pointer to value
	          // Maths
	IAdd,        // Adds two integers on the operand stack
	ISub,        // Substract two integers on the operand stack
	ILessThanEq, // Compare two integers on the operand stack
	PtrOffset,
	             // Debug
	DebugLabel, // Inline debug string
	Count,
};
inline const char *OpCode_str[] = {
	"LoadConstantU32",
	"LoadConstantStr",
	"Call",
	"CallForeign",
	"Ret",
	"ConditionalJump",
	"Jump",
	"Halt",
	"StoreLocal",
	"LoadLocal",
	"StackAlloc",
	"Load",
	"Store",
	"IAdd",
	"ISub",
	"ILessThanEq",
	"PtrOffset",
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
static_assert(ARRAY_LENGTH(BuiltinTypeKind_str) == uint32_t(BuiltinTypeKind::Count));

inline constexpr uint32_t BuiltinTypeKind_size[] = {
	0,
	4,
	1,
	4,
	4,
	8,
};
static_assert(ARRAY_LENGTH(BuiltinTypeKind_size) == uint32_t(BuiltinTypeKind::Count));

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
TypeID type_id_pointer_from(TypeID inner_type);
TypeID type_id_deref_pointer(TypeID pointer_type);
bool type_similar(TypeID lhs_id, TypeID rhs_id);
uint32_t type_get_size(Module *module, TypeID id);


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
	uint32_t address; // offset into the compiler bytecode
	TypeID arg_types[MAX_ARGUMENTS];
	uint32_t arg_count;
	TypeID return_type;
	bool is_foreign;
};

struct Module
{
	sv name;
	Function *functions;
	uint32_t functions_capacity;
	uint32_t functions_length;
	uint8_t *bytecode;
	uint32_t bytecode_capacity;
	uint32_t bytecode_length;
	UserDefinedType *types;
	uint32_t types_capacity;
	uint32_t types_length;
	sv *constant_strings;
	uint32_t constant_strings_capacity;
	uint32_t constant_strings_length;
	uint32_t *constants_u32;
	uint32_t constants_u32_capacity;
	uint32_t constants_u32_length;
};

struct RuntimeError
{
	sv message;
	sv file;
	int line;
	uint64_t ip;
};

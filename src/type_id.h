#pragma once

typedef struct CompilerModule CompilerModule;

// Primitive types
enum BuiltinTypeKind
{
	BuiltinTypeKind_Unit,
	BuiltinTypeKind_Int,
	BuiltinTypeKind_Bool,
	BuiltinTypeKind_Float,
	BuiltinTypeKind_Pointer,
	BuiltinTypeKind_Str,
	BuiltinTypeKind_Count,
};
typedef enum BuiltinTypeKind BuiltinTypeKind;

const char *BuiltinTypeKind_str[] = {
	"()",
	"i32",
	"bool",
	"f32",
	"ptr",
	"str",
};
const uint32_t BuiltinTypeKind_size[] = {
	0,
	4,
	1,
	4,
	8,
	8,
};	
_Static_assert((uint8_t)(BuiltinTypeKind_Count) < 16u, "Should fit on 4 bits.");
_Static_assert(ARRAY_LENGTH(BuiltinTypeKind_str) == (uint32_t)(BuiltinTypeKind_Count));
_Static_assert(ARRAY_LENGTH(BuiltinTypeKind_size) == (uint32_t)(BuiltinTypeKind_Count));

// Pointer type
typedef struct PointerTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind builtin_kind : 4; // BuiltinTypeKind_Pointer
	BuiltinTypeKind pointee_builtin_kind : 4;
	uint32_t indirection_count : 4;
	uint32_t user_defined_index : 19;
} PointerTypeID;
// Primitive type
typedef struct BuiltinTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind kind : 4;
	uint32_t padding : 27;
} BuiltinTypeID;
// User-defined type
typedef struct UserDefinedTypeID
{
	uint32_t is_user_defiend : 1;
	uint32_t index : 31;
} UserDefinedTypeID;

// Type descriptor
typedef union TypeID
{
	PointerTypeID pointer;
	BuiltinTypeID builtin;
	UserDefinedTypeID user_defined;
	uint32_t raw;
} TypeID;
_Static_assert(sizeof(PointerTypeID) == sizeof(uint32_t), "is a uint32_t");
_Static_assert(sizeof(BuiltinTypeID) == sizeof(uint32_t), "is a uint32_t");
_Static_assert(sizeof(UserDefinedTypeID) == sizeof(uint32_t), "is a uint32_t");
_Static_assert(sizeof(TypeID) == sizeof(uint32_t), "is a uint32_t");

inline bool type_id_is_pointer(TypeID id)
{
	return id.builtin.is_user_defined == 0 && id.builtin.kind == BuiltinTypeKind_Pointer;
}
	
inline bool type_id_is_builtin(TypeID id)
{
	return id.builtin.is_user_defined == 0 && id.builtin.kind != BuiltinTypeKind_Pointer;
}
	
inline bool type_id_is_user_defined(TypeID id)
{
	return id.builtin.is_user_defined == 1;
}
	
TypeID type_id_new_builtin(BuiltinTypeKind kind)
{
	TypeID id = {0};
	id.builtin.is_user_defined = 0;
	id.builtin.kind = kind;
	id.builtin.padding = 0;
	return id;
}

const TypeID UNIT_TYPE = {.builtin.kind = BuiltinTypeKind_Unit};

inline TypeID type_id_new_user_defined(uint32_t index)
{
	TypeID id = {0};
	id.user_defined.is_user_defiend = 1;
	id.user_defined.index = index;
	return id;
}

inline TypeID type_id_pointer_from(TypeID inner_type)
{
	if (type_id_is_pointer(inner_type)) {
		inner_type.pointer.indirection_count += 1;
		return inner_type;
	} else if (type_id_is_builtin(inner_type)) {
		TypeID pointer_type = {0};
		pointer_type.pointer = (PointerTypeID){0};
		pointer_type.pointer.builtin_kind = BuiltinTypeKind_Pointer;
		pointer_type.pointer.pointee_builtin_kind = inner_type.builtin.kind;
		pointer_type.pointer.indirection_count = 1;
		return pointer_type;
	} else if (type_id_is_user_defined(inner_type)) {
		TypeID pointer_type = {0};
		pointer_type.pointer = (PointerTypeID){0};
		pointer_type.pointer.builtin_kind = BuiltinTypeKind_Pointer;
		pointer_type.pointer.indirection_count = 1;
		pointer_type.pointer.user_defined_index = inner_type.user_defined.index;
		return pointer_type;
	}
	
	return UNIT_TYPE;
}

inline TypeID type_id_deref_pointer(TypeID pointer_type)
{
	if (pointer_type.pointer.indirection_count > 1) {
		TypeID pointed_type = pointer_type;
		pointed_type.pointer.indirection_count += 1;
		return pointed_type;
	} else if (pointer_type.pointer.pointee_builtin_kind != BuiltinTypeKind_Unit) {
		return type_id_new_builtin(pointer_type.pointer.pointee_builtin_kind);
	} else {
		return type_id_new_user_defined(pointer_type.pointer.user_defined_index);
	}
}

inline bool type_similar(TypeID lhs_id, TypeID rhs_id)
{
	return lhs_id.raw == rhs_id.raw;
}

uint32_t type_get_size(CompilerModule *module, TypeID id);


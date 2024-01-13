#pragma once

typedef struct CompilerModule CompilerModule;

// Primitive types
enum BuiltinTypeKind
{
	BuiltinTypeKind_Unit,
	BuiltinTypeKind_Signed,
	BuiltinTypeKind_Unsigned,
	BuiltinTypeKind_Bool,
	BuiltinTypeKind_Float,
	BuiltinTypeKind_Pointer,
	BuiltinTypeKind_Str,
	BuiltinTypeKind_Count,
};
typedef enum BuiltinTypeKind BuiltinTypeKind;

// Pointer type
typedef struct PointerTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind builtin_kind : 4; // BuiltinTypeKind_Pointer
	BuiltinTypeKind pointee_builtin_kind : 4;
	uint32_t pointee_number_width : 2;
	uint32_t indirection_count : 4;
	uint32_t user_defined_index : 15;
} PointerTypeID;
// Primitive type
typedef enum NumberWidth {
	NumberWidth_8,
	NumberWidth_16,
	NumberWidth_32,
	NumberWidth_64,
} NumberWidth;
typedef struct BuiltinTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind kind : 4;
	uint32_t number_width : 2;
	uint32_t padding : 25;
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

TypeID type_id_new_signed(NumberWidth width)
{
	TypeID id = {0};
	id.builtin.is_user_defined = 0;
	id.builtin.kind = BuiltinTypeKind_Signed;
	id.builtin.number_width = (uint32_t)width;
	id.builtin.padding = 0;
	return id;
}

TypeID type_id_new_unsigned(NumberWidth width)
{
	TypeID id = {0};
	id.builtin.is_user_defined = 0;
	id.builtin.kind = BuiltinTypeKind_Unsigned;
	id.builtin.number_width = (uint32_t)width;
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
		pointer_type.pointer.pointee_number_width = inner_type.builtin.number_width;
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
		TypeID type_id = type_id_new_builtin(pointer_type.pointer.pointee_builtin_kind);
		type_id.builtin.number_width = pointer_type.pointer.pointee_number_width;
		return type_id;
	} else {
		return type_id_new_user_defined(pointer_type.pointer.user_defined_index);
	}
}

/*
  Notes on integer conversion:
trivial
u8 -> i16 u16 i32 u32 i64 u64
u16 -> i32 u32 i64 u64
u32 -> i64 u64
i8 -> i16 i32 i64
i16 -> i32 i64
i32 -> i64

runtime range check (smaller than 0x7F...)
u8 -> i8
u16 -> i16
u32 -> i32
u64 -> i64

runtime check positive
i8 -> u8 u16 u32 u64
i16 -> u16 u32 u64
i32 -> u32 u64
i64 -> u64

literal type:
- unary minus: check bits length, iX
- otherwise: check bits length, check half length
*/

inline bool type_similar(TypeID operand_id, TypeID expected_id)
{
	if (type_id_is_builtin(operand_id) && type_id_is_builtin(expected_id)) {
		// Unsigned conversions
		if (operand_id.builtin.kind == BuiltinTypeKind_Unsigned) {
			if (expected_id.builtin.kind == BuiltinTypeKind_Unsigned) {
				// An unsigned number is compatible with a wider unsigned number.
				return operand_id.builtin.number_width <= expected_id.builtin.number_width;
			}
			else if (expected_id.builtin.kind == BuiltinTypeKind_Signed) {
				// An unsigned number is compatible with stricly wider signed.
				return operand_id.builtin.number_width < expected_id.builtin.number_width;
			}
		}
	}
	return operand_id.raw == expected_id.raw;
}

uint32_t type_get_size(CompilerModule *module, TypeID id);
void type_build_string(StringBuilder *sb, TypeID id);

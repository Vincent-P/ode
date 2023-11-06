#pragma once

struct CompilerModule;

// Primitive types
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
inline constexpr const char *BuiltinTypeKind_str[] = {
	"()",
	"i32",
	"bool",
	"f32",
	"ptr",
	"str",
};
inline constexpr uint32_t BuiltinTypeKind_size[] = {
	0,
	4,
	1,
	4,
	8,
	8,
};	
static_assert(uint8_t(BuiltinTypeKind::Count) < 16u, "Should fit on 4 bits.");
static_assert(ARRAY_LENGTH(BuiltinTypeKind_str) == uint32_t(BuiltinTypeKind::Count));
static_assert(ARRAY_LENGTH(BuiltinTypeKind_size) == uint32_t(BuiltinTypeKind::Count));

// Pointer type
struct PointerTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind builtin_kind : 4; // BuiltinTypeKind::Pointer
	BuiltinTypeKind pointee_builtin_kind : 4;
	uint32_t indirection_count : 4;
	uint32_t user_defined_index : 19;
};
// Primitive type
struct BuiltinTypeID
{
	uint32_t is_user_defined : 1;
	BuiltinTypeKind kind : 4;
	uint32_t padding : 27;
};
// User-defined type
struct UserDefinedTypeID
{
	uint32_t is_user_defiend : 1;
	uint32_t index : 31;
};
// Type descriptor
union TypeID
{
	PointerTypeID pointer;
	BuiltinTypeID builtin;
	UserDefinedTypeID user_defined;
	uint32_t raw;
};
static_assert(sizeof(PointerTypeID) == sizeof(uint32_t), "is a uint32_t");
static_assert(sizeof(BuiltinTypeID) == sizeof(uint32_t), "is a uint32_t");
static_assert(sizeof(UserDefinedTypeID) == sizeof(uint32_t), "is a uint32_t");
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

constexpr TypeID UNIT_TYPE = type_id_new_builtin(BuiltinTypeKind::Unit);

inline TypeID type_id_new_user_defined(uint32_t index)
{
	TypeID id = {};
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
		TypeID pointer_type = {};
		pointer_type.pointer = {};
		pointer_type.pointer.builtin_kind = BuiltinTypeKind::Pointer;
		pointer_type.pointer.pointee_builtin_kind = inner_type.builtin.kind;
		pointer_type.pointer.indirection_count = 1;
		return pointer_type;
	} else if (type_id_is_user_defined(inner_type)) {
		TypeID pointer_type = {};
		pointer_type.pointer = {};
		pointer_type.pointer.builtin_kind = BuiltinTypeKind::Pointer;
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
	} else if (pointer_type.pointer.pointee_builtin_kind != BuiltinTypeKind::Unit) {
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


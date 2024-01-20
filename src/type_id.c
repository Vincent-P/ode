#include "type_id.h"

static void builtin_type_build_string(StringBuilder *sb, BuiltinTypeKind kind, uint32_t number_width)
{
	const char *BuiltinTypeKind_str[] = {
		"()",
		"i",
		"u",
		"bool",
		"f32",
		"ptr",
		"[]",
	};
	_Static_assert((uint8_t)(BuiltinTypeKind_Count) < 16u, "Should fit on 4 bits.");
	_Static_assert(ARRAY_LENGTH(BuiltinTypeKind_str) == (uint32_t)(BuiltinTypeKind_Count));

	string_builder_append_sv(sb, sv_from_null_terminated(BuiltinTypeKind_str[kind]));
	if (kind == BuiltinTypeKind_Signed || kind == BuiltinTypeKind_Unsigned) {
		string_builder_append_u64(sb, (1<<number_width)*8);
	}
}

static void user_defined_type_build_string(StringBuilder *sb, uint32_t user_defined_index)
{
	string_builder_append_sv(sb, SV("<user_defined#"));
	string_builder_append_u64(sb, user_defined_index);
	string_builder_append_char(sb, '>');
}


void type_build_string(StringBuilder *sb, TypeID id)
{
	if (id.builtin.is_user_defined)
	{
		user_defined_type_build_string(sb, id.user_defined.index);
	}
	else if (id.builtin.kind == BuiltinTypeKind_Pointer)
	{
		for (uint32_t i = 0; i < id.pointer.indirection_count; ++i) {
			string_builder_append_char(sb, '*');
		}
		if (id.pointer.builtin_kind != BuiltinTypeKind_Unit) {
			builtin_type_build_string(sb, id.pointer.pointee_builtin_kind, id.pointer.pointee_number_width);
		}
		else {
			user_defined_type_build_string(sb, id.pointer.user_defined_index);
		}
	}
	else if (id.builtin.kind == BuiltinTypeKind_Slice)
	{
		string_builder_append_char(sb, '[');
		string_builder_append_char(sb, ']');
		for (uint32_t i = 1; i < id.slice.indirection_count; ++i) {
			string_builder_append_char(sb, '*');
		}
		if (id.slice.builtin_kind != BuiltinTypeKind_Unit) {
			builtin_type_build_string(sb, id.slice.pointee_builtin_kind, id.slice.pointee_number_width);
		}
		else {
			user_defined_type_build_string(sb, id.slice.user_defined_index);
		}
	}
	else
	{
		builtin_type_build_string(sb, id.builtin.kind, id.builtin.number_width);
	}
}

#pragma once

enum Constants
{
	MAX_STRUCT_FIELDS = 8, // Maximum number of fields in a user defined type
	TYPE_MAX_USER_DEFINED = (1 << 19), // match the user_defined_index
	MAX_ARGUMENTS = 8, // Maximum number of arguments of a function
	SCOPE_MAX_VARIABLES = 16,
};

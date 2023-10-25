#pragma once
#include "core.h"
#include "type_id.h"
#include "parser.h"

struct Module;


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
	CallExternal, // Read the immediate index of the moduel, and function to jump to, and creates a callstack
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
	"CallExternal",
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

struct UserDefinedType
{
	sv name;
	sv field_names[MAX_STRUCT_FIELDS];
	TypeID field_types[MAX_STRUCT_FIELDS];
	uint32_t field_offsets[MAX_STRUCT_FIELDS];
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
	// functions containes in this module
	Function *functions;
	uint32_t functions_capacity;
	uint32_t functions_length;
	// bytecode for all the functions
	uint8_t *bytecode;
	uint32_t bytecode_capacity;
	uint32_t bytecode_length;
	// types defined in this module
	UserDefinedType *types;
	uint32_t types_capacity;
	uint32_t types_length;
	// table of constants string literal
	sv *constant_strings;
	uint32_t constant_strings_capacity;
	uint32_t constant_strings_length;
	// table of numeric constants
	uint32_t *constants_u32;
	uint32_t constants_u32_capacity;
	uint32_t constants_u32_length;
	// table of imports
	uint32_t *imports;
	uint32_t imports_capacity;
	uint32_t imports_length;
};

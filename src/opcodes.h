#pragma once
#include "core.h"

enum OpCode
{
	// Halt the execution, set to 0 so that uninitialized bytecode halts.
	OpCode_Halt = 0,
	// Do nothing.
	OpCode_Nop,
	// PushU32 <constant:i32>
	OpCode_PushU32,
	// PushStr <constant_offset:u32>
	OpCode_PushStr,
	// Call <address:u32> <num_args:u8> (arg0, arg1, ..., argN)
	OpCode_Call,
	// Call <i_imported_function:i8> <num_args:u8> (arg0, arg1, ..., argN)
	OpCode_CallInModule,
	// Call <i_foreign_function:i8> <num_args:u8> (arg0, arg1, ..., argN)
	OpCode_CallForeign,
	// Ret (value)
	OpCode_Ret,
	// ConditionalJump <offset:i32> (condition)
	OpCode_ConditionalJump,
	// Jump <offset:i32>
	OpCode_Jump,
	// Set a value of an argument (before the function stack)
	// StoreArg <i_arg:i8> (value)
	OpCode_StoreArg,
	// Get a value of an argument
	// LoadArg <i_arg:i8>
	OpCode_LoadArg,
	// Set a value starting from the function stack
	// StoreLocal <i_local:i8> (value)
	OpCode_StoreLocal,
	// Get a value starting from the function stack
	// LoadLocal <i_local:i8>
	OpCode_LoadLocal,
	// -- Pointers
	// Load (ptr)
	OpCode_Load8,
	OpCode_Load16,
	OpCode_Load32,
	// Store (ptr, value)
	OpCode_Store8,
	OpCode_Store16,
	OpCode_Store32,
	// -- Integers arithmetic
	// Add (arg0, arg1)
	OpCode_AddU8,
	OpCode_AddU16,
	OpCode_AddU32,
	// Sub (arg0, arg1)
	OpCode_SubU8,
	OpCode_SubU16,
	OpCode_SubU32,
	// LteI32 (arg0, arg1)
	OpCode_LteI32,
	// GteI32 (arg0, arg1)
	OpCode_GteI32,
	// EqI32 (arg0, arg1)
	OpCode_EqI32,
	// -- Logic
	// Binary AND
	OpCode_And,
	// Debug instruction with a string
	OpCode_DebugLabel,
	OpCode_Count,
};
typedef enum OpCode OpCode;

const char *OpCode_str[] = {
	"Halt",
	"Nop",
	"PushU32",
	"PushStr",
	"Call",
	"CallInModule",
	"CallForeign",
	"Ret",
	"ConditionalJump",
	"Jump",
	"StoreArg",
	"LoadArg",
	"StoreLocal",
	"LoadLocal",
	"Load8",
	"Load16",
	"Load32",
	"Store8",
	"Store16",
	"Store32",
	"AddU8",
	"AddU16",
	"AddU32",
	"SubU8",
	"SubU16",
	"SubU32",
	"LteI32",
	"GteI32",
	"EqI32",
	"And",
	"DebugLabel",
};
_Static_assert(ARRAY_LENGTH(OpCode_str) == (uint8_t)(OpCode_Count));

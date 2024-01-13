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
	// -- Integers
	// LoadU32 (ptr)
	OpCode_Load32,
	// StoreU32 (ptr, value)
	OpCode_Store32,
	// AddI32 (arg0, arg1)
	OpCode_AddI32,
	// SubI32 (arg0, arg1)
	OpCode_SubI32,
	// LteI32 (arg0, arg1)
	OpCode_LteI32,
	// GteI32 (arg0, arg1)
	OpCode_GteI32,
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
	"Load32",
	"Store32",
	"AddI32",
	"SubI32",
	"LteI32",
	"GteI32",
	"And",
	"DebugLabel",
};
_Static_assert(ARRAY_LENGTH(OpCode_str) == (uint8_t)(OpCode_Count));

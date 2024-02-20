#pragma once
#include "core.h"

enum OpCode
{
	// Halt the execution, set to 0 so that uninitialized bytecode halts.
	OpCode_Halt = 0,
	// Do nothing.
	OpCode_Nop,
	// -- Stack manipulation
	// PushU32 <constant:i32>
	OpCode_PushU32,
	// PushF32 <constant:f32>
	OpCode_PushF32,
	// PushStr <constant_offset:u32>
	OpCode_PushStr,
	OpCode_Pop,
	OpCode_Swap,
	// -- Control flow
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
	// -- Args/Locals management
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
	OpCode_Load64,
	// Store (ptr, value)
	OpCode_Store8,
	OpCode_Store16,
	OpCode_Store32,
	// -- Slice
	OpCode_SliceData,
	OpCode_SliceLength,
	// -- Integers arithmetic
	// Mul (arg0, arg1)
	OpCode_MulI32,
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
	OpCode_LtI32,
	// GteI32 (arg0, arg1)
	OpCode_GteI32,
	OpCode_GtI32,
	// EqI32 (arg0, arg1)
	OpCode_EqI32,
	// Floats (arg0, arg1)
	OpCode_MulF32,
	OpCode_AddF32,
	OpCode_SubF32,
	OpCode_LtF32,
	OpCode_LteF32,
	OpCode_GtF32,
	OpCode_GteF32,
	OpCode_EqF32,
	// -- Logic
	// Binary AND (arg0, arg1)
	OpCode_And,
	// Binary OR (arg0, arg1)
	OpCode_Or,
	// Debug instruction with a string
	OpCode_DebugLabel,
	OpCode_Count,
};
typedef enum OpCode OpCode;

const char *OpCode_str[] = {
	"Halt",
	"Nop",
	"PushU32",
	"PushF32",
	"PushStr",
	"Pop",
	"Swap",
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
	"Load64",
	"Store8",
	"Store16",
	"Store32",
	"SliceData",
	"SliceLength",
	"MulI32",
	"AddU8",
	"AddU16",
	"AddU32",
	"SubU8",
	"SubU16",
	"SubU32",
	"LteI32",
	"LtI32",
	"GteI32",
	"GtI32",
	"EqI32",
	"MulF32",
	"AddF32",
	"SubF32",
	"LtF32",
	"LteF32",
	"GtF32",
	"GteF32",
	"EqF32",
	"And",
	"Or",
	"DebugLabel",
};
_Static_assert(ARRAY_LENGTH(OpCode_str) == (uint8_t)(OpCode_Count));

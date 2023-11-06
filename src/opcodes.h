#pragma once
#include "core.h"

enum struct OpCode : uint8_t
{
	// Halt the execution, set to 0 so that uninitialized bytecode halts.
	Halt = 0,
	// Do nothing.
	Nop,
	// PushU32 <constant:i32>
	PushU32,
	// Call <i_function:i8> <num_args:u8> (arg0, arg1, ..., argN)
	Call,
	// Call <i_module:i8> <i_function:i8> <num_args:u8> (arg0, arg1, ..., argN)
	CallInModule,
	// Ret (value)
	Ret,
	// ConditionalJump <offset:i32> (condition)
	ConditionalJump,
	// Jump <offset:i32>
	Jump,
	// Set a value of an argument (before the function stack)
	// StoreArg <i_arg:i8> (value)
	StoreArg,
	// Get a value of an argument
	// LoadArg <i_arg:i8>
	LoadArg,
	// Set a value starting from the function stack
	// StoreLocal <i_local:i8> (value)
	StoreLocal,
	// Get a value starting from the function stack
	// LoadLocal <i_local:i8>
	LoadLocal,
	// LoadU32 (ptr)
	Load32,
	// StoreU32 (ptr, value)
	Store32,
	// AddI32 (arg0, arg1)
	AddI32,
	// SubI32 (arg0, arg1)
	SubI32,
	// LteI32 (arg0, arg1)
	LteI32,
	// Debug instruction with a string
	DebugLabel,
	Count,
};

inline const char *OpCode_str[] = {
	"Halt",
	"Nop",
	"PushU32",
	"Call",
	"CallInModule",
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
	"DebugLabel",
};
static_assert(ARRAY_LENGTH(OpCode_str) == uint8_t(OpCode::Count));

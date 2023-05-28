#pragma once
#include "core.h"

enum OpCode : uint8_t
{
	// Stack manipulation
	Constant,
	Push,
	// Control flow
	Ret,
	// Variables storage
	BeginScope,
	EndScope,
	SetLocal,
	GetLocal,
	// Maths
	IAdd,
	ILessThanEq,
	Halt,
	Count,
};
inline const char *OpCode_str[] = {
	"Constant",
	"Push",
	"Ret",
	"BeginScope",
	"EndScope",
	"SetLocal",
	"GetLocal",
	"IAdd",
	"ILessThanEq",
	"Halt",
};
static_assert(ARRAY_LENGTH(OpCode_str) == uint8_t(OpCode::Count));

struct InstructionBank
{
	OpCode bytecode[1024];
};

struct Image
{
	InstructionBank *instruction_banks;
	uint64_t instruction_banks_length;
	uint8_t *memory;
	uint64_t memory_length;
};

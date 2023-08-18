#pragma once
#include "core.h"
#include "image.h"

struct RuntimeError;
struct Image;
struct ExecutorState;
struct Module;
struct ExecutionContext;

using ForeignFn = void (*)(ExecutionContext *);

struct ExecutorConfig
{
	void (*error_callback)(ExecutorState *, RuntimeError);
	ForeignFn (*foreign_callback)(sv module_name, sv function_name);
};

ExecutorState *executor_init(ExecutorConfig config);
void executor_load_module(ExecutorState *state, Module *module);
void executor_execute_module_entrypoint(ExecutorState *state, sv module_name);

// Representation of a builtin pointer
struct TypedPointer
{
	TypeID type_id;
	uint32_t offset;
};

// Representation of a builtin str
struct Str
{
	uint32_t is_constant : 1;
	uint32_t offset : 31;
	uint32_t length;
};

// Operand stack
enum struct StackValueKind : uint8_t
{
	Invalid,
	Bool, // a bool constant
	Float, // a float constant
	I32, // an int32 constant
	Str, // a str constant
	LocalPtr, // a pointer to local memory
	LocalObject, // an object in local memory
	Count,
};

struct StackValue
{
	StackValueKind kind;
	union
	{
		// constants values
		bool b8;
		float f32;
		int32_t i32;
		Str str;
		// runtime values
		TypedPointer local_ptr;
		TypedPointer local_object;
	};
};

inline bool stack_value_is_constant(StackValue v) { return v.kind != StackValueKind::LocalPtr && v.kind != StackValueKind::LocalObject; }
inline StackValue stack_value_bool(bool v) { StackValue res = {}; res.kind = StackValueKind::Bool; res.b8 = v; return res; }
inline StackValue stack_value_float(float v) { StackValue res = {}; res.kind = StackValueKind::Float; res.f32 = v; return res; }
inline StackValue stack_value_i32(int32_t v) { StackValue res = {}; res.kind = StackValueKind::I32; res.i32 = v; return res; }
inline StackValue stack_value_str(Str v) { StackValue res = {}; res.kind = StackValueKind::Str; res.str = v; return res; }
inline StackValue stack_value_local_ptr(TypedPointer v) { StackValue res = {}; res.kind = StackValueKind::LocalPtr; res.local_ptr = v; return res; }
inline StackValue stack_value_local_object(TypedPointer v) { StackValue res = {}; res.kind = StackValueKind::LocalObject; res.local_object = v; return res; }


StackValue execution_get_local(ExecutionContext *ctx, uint32_t i_local);
sv execution_get_str(ExecutionContext *ctx, Str str);

#include "executor.h"
#include "opcodes.h"
#include "cross.h"

static const char* PointerType_str[] = {
	"PointerType_Host",
	"PointerType_Image",
	"PointerType_Heap",
};
_Static_assert(ARRAY_LENGTH(PointerType_str) == PointerType_Count);

static uint8_t bytecode_read_u8(ExecutionContext *ctx, uint32_t mp, uint32_t *ip)
{
	uint8_t *bytecode = ctx->modules[mp].bytecode;
	uint8_t *result = bytecode + *ip;
	*ip = *ip + sizeof(uint8_t);
	return *result;
}

static uint32_t bytecode_read_u32(ExecutionContext *ctx, uint32_t mp, uint32_t *ip)
{
	uint8_t *bytecode = ctx->modules[mp].bytecode;
	uint32_t *result = (uint32_t*)(bytecode + *ip);
	*ip = *ip + sizeof(uint32_t);
	return *result;
}

static void push_n(ExecutionContext *ctx, uint32_t *_sp, Value *values, uint32_t values_len)
{
	// SP always point to the top of the stack (the latest pushed element)
	uint32_t sp = *_sp + 1;
	Value *stack = ctx->stack;

	for (uint32_t i_val = 0; i_val < values_len; ++i_val) {
		stack[sp + i_val] = values[i_val];
	}

	*_sp = sp + values_len - 1;
}
static void push(ExecutionContext *ctx, uint32_t *sp, Value val)
{
	push_n(ctx, sp, &val, 1);
}

static void pop_n(ExecutionContext *ctx, uint32_t *_sp, Value *values, uint32_t values_len)
{
	uint32_t sp = *_sp;
	Value *stack = ctx->stack;

	for (uint32_t i_val = 0; i_val < values_len; ++i_val) {
		values[i_val] = stack[sp];
		sp -= 1;
	}

	*_sp = sp;
}
static Value pop(ExecutionContext *ctx, uint32_t *sp)
{
	Value val;
	pop_n(ctx, sp, &val, 1);
	return val;
}

static void debug_print_stack(ExecutionContext *ctx, uint32_t sp, uint32_t bp)
{
	char logbuf[64] = {0};
	
	cross_log(cross_stderr, sv_from_null_terminated("[DEBUG] Stack:\n"));
	
	for (uint32_t i = 0; i <= sp && i < ARRAY_LENGTH(ctx->stack); ++i) {
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_char(&sb, '[');
		string_builder_append_u64(&sb, (uint64_t)(i));
		string_builder_append_sv(&sb, SV("] u32 = "));
		string_builder_append_u64(&sb, (uint64_t)(ctx->stack[i].u32));
		string_builder_append_sv(&sb, SV(" | f32 = "));
		string_builder_append_f32(&sb, ctx->stack[i].f32);
		if (i == bp) {
			string_builder_append_sv(&sb, SV("  <-- bp"));
		}
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
	}
}


void call_function(
		   ExecutionContext *ctx, uint32_t callee_module, uint32_t callee_ip, Value *args, uint32_t args_len)
{
	char logbuf[128] = {0};
	// Registers
	uint32_t cp = 0;                        // callstack pointer
	uint32_t sp = (uint32_t)(-1);             // stack pointer
	uint32_t bp = args_len - 1;             // base stack pointer
	uint32_t ip = callee_ip;                // instruction pointer
	uint32_t mp = callee_module;            // module pointer

	ctx->callstack_ret_module[cp] = ~(uint32_t)(0);
	ctx->callstack_ret_address[cp] = ~(uint32_t)(0);
	ctx->callstack_ret_bp[cp] = ~(uint32_t)(0);
	ctx->callstack_argc[cp] = args_len;

	push_n(ctx, &sp, args, args_len);

	while (true) {
		if (mp >= ctx->modules_len || ip >= ctx->modules[mp].bytecode_len) {
			break;
		}

		OpCode op = (OpCode)bytecode_read_u8(ctx, mp, &ip);
		
		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_sv(&sb, SV("[TRACE] "));
		string_builder_append_sv(&sb, SV(OpCode_str[(uint8_t)(op)]));
		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
		
		switch (op) {
		case OpCode_Halt: {
			cross_log(cross_stderr, SV("HALT\n"));
			debug_print_stack(ctx, sp, bp);
			__debugbreak();
			return;
			break;
		}
		case OpCode_Nop: {
			break;
		}
		case OpCode_PushU32: {
			Value val;
			val.u32 = bytecode_read_u32(ctx, mp, &ip);
			push(ctx, &sp, val);
			
			string_builder_append_sv(&sb, SV("[DEBUG] | val.u32 = "));
			string_builder_append_u64(&sb, (uint64_t)(val.u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));

			break;
		}
		case OpCode_PushStr: {
			const uint32_t string_offset = bytecode_read_u32(ctx, mp, &ip);

			const uint8_t *memory = ctx->modules[mp].constants + string_offset;
			const uint32_t *memory_u32 = (uint32_t*)memory;
			const uint32_t string_length = *memory_u32;
			
			Value val;
			val.ptr.type = PointerType_Image;
			val.ptr.offset = string_offset + sizeof(string_length);
			push(ctx, &sp, val);
			
			string_builder_append_sv(&sb, SV("[DEBUG] | str = "));
			string_builder_append_sv(&sb, sv_from_null_terminated(PointerType_str[val.ptr.type]));
			string_builder_append_char(&sb, ' ');
			string_builder_append_u64(&sb, (uint64_t)val.ptr.offset);
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));

			break;
		}
		case OpCode_Call: {
			debug_print_stack(ctx, sp, bp);
			
			uint32_t function_address = bytecode_read_u32(ctx, mp, &ip);
			uint8_t argc = bytecode_read_u8(ctx, mp, &ip);
			// Push callstack
			cp += 1;
			ctx->callstack_ret_module[cp] = mp;
			ctx->callstack_ret_address[cp] = ip;
			ctx->callstack_ret_bp[cp] = bp;
			ctx->callstack_argc[cp] = argc;
			// Jump to function
			ip = function_address;
			bp = sp;

			string_builder_append_sv(&sb, SV("[DEBUG] | function_address = "));
			string_builder_append_u64(&sb, (uint64_t)(function_address));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_CallInModule: {
			debug_print_stack(ctx, sp, bp);
			
			uint8_t i_imported_function = bytecode_read_u8(ctx, mp, &ip);
			uint32_t external_module_address = ctx->modules[mp].import_module[i_imported_function];
			uint32_t external_function_address = (uint32_t)(ctx->modules[mp].import_addresses[i_imported_function]);
			
			uint8_t argc = bytecode_read_u8(ctx, mp, &ip);

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | i_imported_function = "));
			string_builder_append_u64(&sb, (uint64_t)(i_imported_function));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			string_builder_append_sv(&sb, SV("[DEBUG] | external_module_address = "));
			string_builder_append_u64(&sb, (uint64_t)(external_module_address));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			string_builder_append_sv(&sb, SV("[DEBUG] | external_function_address = "));
			string_builder_append_u64(&sb, (uint64_t)(external_function_address));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			string_builder_append_sv(&sb, SV("[DEBUG] | argc = "));
			string_builder_append_u64(&sb, (uint64_t)(argc));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));

			// Push callstack
			cp += 1;
			ctx->callstack_ret_module[cp] = mp;
			ctx->callstack_ret_address[cp] = ip;
			ctx->callstack_ret_bp[cp] = bp;
			ctx->callstack_argc[cp] = argc;
			// Jump to function
			mp = external_module_address;
			ip = external_function_address;
			bp = sp;
			break;
		}
		case OpCode_CallForeign: {
			debug_print_stack(ctx, sp, bp);
			
			uint8_t i_foreign_function = bytecode_read_u8(ctx, mp, &ip);
			uint8_t argc = bytecode_read_u8(ctx, mp, &ip);
			
			ForeignFn callback = ctx->modules[mp].foreign_function_callback[i_foreign_function];

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | i_foreign_function = "));
			string_builder_append_u64(&sb, (uint64_t)(i_foreign_function));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			string_builder_append_sv(&sb, SV("[DEBUG] | callback = "));
			string_builder_append_u64(&sb, (uint64_t)(callback));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			string_builder_append_sv(&sb, SV("[DEBUG] | argc = "));
			string_builder_append_u64(&sb, (uint64_t)(argc));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));

			callback(ctx->stack + sp - argc + 1, argc);
			
			// Pop arguments
			sp = sp - argc;
			break;
		}
		case OpCode_Ret: {
			Value return_value = pop(ctx, &sp);
			// Pop the stack, and the arguments that were before bp
			sp = bp - ctx->callstack_argc[cp];
			// Pop callstack
			mp = ctx->callstack_ret_module[cp];
			ip = ctx->callstack_ret_address[cp];
			bp = ctx->callstack_ret_bp[cp];
			cp -= 1;
			// Push the return value
			push(ctx, &sp, return_value);

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | return_value.u32 = "));
			string_builder_append_u64(&sb, (uint64_t)(return_value.u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));

			debug_print_stack(ctx, sp, bp);			
			break;
		}
		case OpCode_ConditionalJump: {
			uint32_t jump_target = bytecode_read_u32(ctx, mp, &ip);
			Value condition = pop(ctx, &sp);
			if (condition.u32) {
				ip = jump_target;
			}
			break;
		}
		case OpCode_Jump: {
			uint32_t jump_target = bytecode_read_u32(ctx, mp, &ip);
			ip = jump_target;
			break;
		}
		case OpCode_StoreArg: {
			uint8_t i_arg = bytecode_read_u8(ctx, mp, &ip);

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | i_arg = "));
			string_builder_append_u64(&sb, (uint64_t)(i_arg));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));

			// Set the arg to the top of the stack
			uint32_t local_address = bp - ctx->callstack_argc[cp] + i_arg + 1;
			ctx->stack[local_address] = pop(ctx, &sp);
			break;
		}
		case OpCode_LoadArg: {
			uint8_t i_arg = bytecode_read_u8(ctx, mp, &ip);
			// Push the arg on top of the stack
			uint32_t local_address = bp - ctx->callstack_argc[cp] + i_arg + 1;
			push(ctx, &sp, ctx->stack[local_address]);
			
			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | i_arg = "));
			string_builder_append_u64(&sb, (uint64_t)(i_arg));
			string_builder_append_sv(&sb, SV(", pushed.u32 = "));
			string_builder_append_u64(&sb, (uint64_t)(ctx->stack[local_address].u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_StoreLocal: {
			uint8_t i_local = bytecode_read_u8(ctx, mp, &ip);
			// Set the local to the top of the stack
			uint32_t local_address = bp + i_local + 1;
			ctx->stack[local_address] = pop(ctx, &sp);

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | i_local = "));
			string_builder_append_u64(&sb, (uint64_t)(i_local));
			string_builder_append_sv(&sb, SV(", popped.u32 = "));
			string_builder_append_u64(&sb, (uint64_t)(ctx->stack[local_address].u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_LoadLocal: {
			uint8_t i_local = bytecode_read_u8(ctx, mp, &ip);
			// Push the local on top of the stack
			uint32_t local_address = bp + i_local + 1;
			push(ctx, &sp, ctx->stack[local_address]);

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | i_local = "));
			string_builder_append_u64(&sb, (uint64_t)(i_local));
			string_builder_append_sv(&sb, SV(", pushed.u32 = "));
			string_builder_append_u64(&sb, (uint64_t)(ctx->stack[local_address].u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_Load8: {
			Pointer ptr = pop(ctx, &sp).ptr;

			uint8_t *memory = NULL;
			switch (ptr.type) {
				case PointerType_Host: {
					__debugbreak();
					break;
				}
				case PointerType_Image: {
					memory = ctx->modules[mp].constants + ptr.offset;
					break;
				}
				case PointerType_Heap: {
					__debugbreak();
					break;
				}
			}

			Value result = {0};
			result.u8 = *(uint8_t*)memory;
			push(ctx, &sp, result);
			
			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | ptr = "));
			string_builder_append_sv(&sb, sv_from_null_terminated(PointerType_str[ptr.type]));
			string_builder_append_char(&sb, ' ');
			string_builder_append_u64(&sb, (uint64_t)ptr.offset);
			string_builder_append_sv(&sb, SV(" | deref.u8 = "));
			string_builder_append_u64(&sb, (uint64_t)(result.u8));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));			
			break;
		}
		case OpCode_Load16: {
			Pointer ptr = pop(ctx, &sp).ptr;

			uint8_t *memory = NULL;
			switch (ptr.type) {
				case PointerType_Host: {
					__debugbreak();
					break;
				}
				case PointerType_Image: {
					memory = ctx->modules[mp].constants + ptr.offset;
					break;
				}
				case PointerType_Heap: {
					__debugbreak();
					break;
				}
			}

			Value result = {0};
			result.u16 = *(uint16_t*)memory;
			push(ctx, &sp, result);
			
			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | ptr = "));
			string_builder_append_sv(&sb, sv_from_null_terminated(PointerType_str[ptr.type]));
			string_builder_append_char(&sb, ' ');
			string_builder_append_u64(&sb, (uint64_t)ptr.offset);
			string_builder_append_sv(&sb, SV(" | deref.u16 = "));
			string_builder_append_u64(&sb, (uint64_t)(result.u16));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));			
			break;
		}
		case OpCode_Load32: {
			Pointer ptr = pop(ctx, &sp).ptr;

			uint8_t *memory = NULL;
			switch (ptr.type) {
				case PointerType_Host: {
					__debugbreak();
					break;
				}
				case PointerType_Image: {
					memory = ctx->modules[mp].constants + ptr.offset;
					break;
				}
				case PointerType_Heap: {
					__debugbreak();
					break;
				}
			}

			Value result = {0};
			result.u32 = *(uint32_t*)memory;
			push(ctx, &sp, result);
			
			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | ptr = "));
			string_builder_append_sv(&sb, sv_from_null_terminated(PointerType_str[ptr.type]));
			string_builder_append_char(&sb, ' ');
			string_builder_append_u64(&sb, (uint64_t)ptr.offset);
			string_builder_append_sv(&sb, SV(" | deref.u32 = "));
			string_builder_append_u64(&sb, (uint64_t)(result.u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));			
			break;
		}
		case OpCode_Store32: {
			Value operands[2] = {0};
			pop_n(ctx, &sp, operands, 2);

			Pointer ptr = operands[0].ptr;
			uint32_t value = operands[1].u32;

			// DEBUG
			if (ptr.type >= PointerType_Count) {
				__debugbreak();
			}

			uint8_t *memory = NULL;
			switch (ptr.type) {
				case PointerType_Host: {
					__debugbreak();
					break;
				}
				case PointerType_Image: {
					memory = ctx->modules[mp].constants + ptr.offset;
					break;
				}
				case PointerType_Heap: {
					__debugbreak();
					break;
				}
			}

			uint32_t *memory_u32 = (uint32_t*)memory;
			*memory_u32 = value;
				
			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | ptr = "));
			string_builder_append_sv(&sb, sv_from_null_terminated(PointerType_str[ptr.type]));
			string_builder_append_char(&sb, ' ');
			string_builder_append_u64(&sb, (uint64_t)ptr.offset);
			string_builder_append_sv(&sb, SV(", val.u32 = "));
			string_builder_append_u64(&sb, (uint64_t)(operands[0].u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_AddI32: {
			Value operands[2] = {0};
			pop_n(ctx, &sp, operands, 2);
			Value result = {0};
			result.i32 = operands[1].i32 + operands[0].i32;
			push(ctx, &sp, result);

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | arg0 = "));
			string_builder_append_u64(&sb, (uint64_t)(operands[1].u32));
			string_builder_append_sv(&sb, SV(" + arg1 = "));
			string_builder_append_u64(&sb, (uint64_t)(operands[0].u32));
			string_builder_append_sv(&sb, SV(" => "));
			string_builder_append_u64(&sb, (uint64_t)(result.u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_SubI32: {
			Value operands[2] = {0};
			pop_n(ctx, &sp, operands, 2);
			Value result = {0};
			result.i32 = operands[1].i32 - operands[0].i32;
			push(ctx, &sp, result);

			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] | arg0 = "));
			string_builder_append_u64(&sb, (uint64_t)(operands[1].u32));
			string_builder_append_sv(&sb, SV(" + arg1 = "));
			string_builder_append_u64(&sb, (uint64_t)(operands[0].u32));
			string_builder_append_sv(&sb, SV(" => "));
			string_builder_append_u64(&sb, (uint64_t)(result.u32));
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_LteI32: {
			Value operands[2] = {0};
			pop_n(ctx, &sp, operands, 2);
			Value result = {0};
			result.i32 = operands[1].i32 <= operands[0].i32;
			push(ctx, &sp, result);
			break;
		}
		case OpCode_GteI32: {
			Value operands[2] = {0};
			pop_n(ctx, &sp, operands, 2);
			Value result = {0};
			result.i32 = operands[1].i32 >= operands[0].i32;
			push(ctx, &sp, result);
			break;
		}
		case OpCode_EqI32: {
			Value operands[2] = {0};
			pop_n(ctx, &sp, operands, 2);
			Value result = {0};
			result.i32 = operands[1].i32 == operands[0].i32;
			push(ctx, &sp, result);
			break;
		}
		case OpCode_And: {
			Value operands[2] = {0};
			pop_n(ctx, &sp, operands, 2);
			Value result = {0};
			result.u32 = operands[1].u32 & operands[0].u32;
			push(ctx, &sp, result);
			break;
		}
		case OpCode_DebugLabel: {
			char debug_buffer[64] = {0};
			uint32_t string_length = bytecode_read_u32(ctx, mp, &ip);
			for (uint32_t i = 0; i < string_length && i < 64; ++i) {
				debug_buffer[i] = (char)bytecode_read_u8(ctx, mp, &ip);
			}
			
			// debug print
			string_builder_append_sv(&sb, SV("[DEBUG] "));
			string_builder_append_sv(&sb, (sv){debug_buffer, string_length});
			string_builder_append_char(&sb, '\n');
			cross_log(cross_stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode_Count: {
			break;
		}
		}
	}
}

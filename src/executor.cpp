#include "executor.h"
#include "opcodes.h"
#include "cross.h"

template <typename T>
static T bytecode_read_scalar(ExecutionContext *ctx, uint32_t mp, uint32_t *ip)
{
	uint8_t *bytecode = ctx->modules[mp].bytecode;
	T *bytecode_as_t = reinterpret_cast<T *>(bytecode + *ip);
	*ip = *ip + sizeof(T);
	return *bytecode_as_t;
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
	char logbuf[64] = {};
	
	cross::log(cross::stderr, sv_from_null_terminated("[DEBUG] Stack:\n"));
	
	for (uint32_t i = 0; i <= sp && i < ARRAY_LENGTH(ctx->stack); ++i) {
		StringBuilder sb = string_builder_from_buffer(logbuf);
		string_builder_append(&sb, '[');
		string_builder_append(&sb, uint64_t(i));
		string_builder_append(&sb, SV("] u32 = "));
		string_builder_append(&sb, uint64_t(ctx->stack[i].u32));
		string_builder_append(&sb, SV(" | f32 = "));
		string_builder_append(&sb, uint64_t(ctx->stack[i].f32));
		if (i == bp) {
			string_builder_append(&sb, SV("  <-- bp"));
		}
		string_builder_append(&sb, '\n');
		cross::log(cross::stderr, string_builder_get_string(&sb));
	}
}


void call_function(
		   ExecutionContext *ctx, uint32_t callee_module, uint32_t callee_ip, Value *args, uint32_t args_len)
{
	char logbuf[128] = {};
	// Registers
	uint32_t cp = 0;                        // callstack pointer
	uint32_t sp = uint32_t(-1);             // stack pointer
	uint32_t bp = args_len - 1;             // base stack pointer
	uint32_t ip = callee_ip;                // instruction pointer
	uint32_t mp = callee_module;            // module pointer

	ctx->callstack_ret_module[cp] = ~uint32_t(0);
	ctx->callstack_ret_address[cp] = ~uint32_t(0);
	ctx->callstack_ret_bp[cp] = ~uint32_t(0);
	ctx->callstack_argc[cp] = args_len;

	push_n(ctx, &sp, args, args_len);

	while (true) {
		if (mp >= ctx->modules_len || ip >= ctx->modules[mp].bytecode_len) {
			break;
		}

		OpCode op = bytecode_read_scalar<OpCode>(ctx, mp, &ip);
		
		StringBuilder sb = string_builder_from_buffer(logbuf);
		string_builder_append(&sb, SV("[TRACE] "));
		string_builder_append(&sb, SV(OpCode_str[uint8_t(op)]));
		string_builder_append(&sb, '\n');
		cross::log(cross::stderr, string_builder_get_string(&sb));

		switch (op) {
		case OpCode::Halt: {
			cross::log(cross::stderr, SV("HALT\n"));
			debug_print_stack(ctx, sp, bp);
			__debugbreak();
			return;
			break;
		}
		case OpCode::Nop: {
			break;
		}
		case OpCode::PushU32: {
			Value val;
			val.u32 = bytecode_read_scalar<uint32_t>(ctx, mp, &ip);
			push(ctx, &sp, val);
			
			string_builder_append(&sb, SV("[DEBUG] | val.u32 = "));
			string_builder_append(&sb, uint64_t(val.u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));

			break;
		}
		case OpCode::Call: {
			debug_print_stack(ctx, sp, bp);
			
			uint32_t function_address = bytecode_read_scalar<uint32_t>(ctx, mp, &ip);
			uint8_t argc = bytecode_read_scalar<uint8_t>(ctx, mp, &ip);
			// Push callstack
			cp += 1;
			ctx->callstack_ret_module[cp] = mp;
			ctx->callstack_ret_address[cp] = ip;
			ctx->callstack_ret_bp[cp] = bp;
			ctx->callstack_argc[cp] = argc;
			// Jump to function
			ip = function_address;
			bp = sp;
			break;
		}
		case OpCode::CallInModule: {
			debug_print_stack(ctx, sp, bp);
			
			uint8_t i_imported_function = bytecode_read_scalar<uint8_t>(ctx, mp, &ip);
			uint32_t external_module_address = ctx->modules[mp].import_module[i_imported_function];
			uint32_t external_function_address = uint32_t(ctx->modules[mp].import_addresses[i_imported_function]);
			
			uint8_t argc = bytecode_read_scalar<uint8_t>(ctx, mp, &ip);

			// debug print
			string_builder_append(&sb, SV("[DEBUG] | i_imported_function = "));
			string_builder_append(&sb, uint64_t(i_imported_function));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			string_builder_append(&sb, SV("[DEBUG] | external_module_address = "));
			string_builder_append(&sb, uint64_t(external_module_address));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			string_builder_append(&sb, SV("[DEBUG] | external_function_address = "));
			string_builder_append(&sb, uint64_t(external_function_address));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			string_builder_append(&sb, SV("[DEBUG] | argc = "));
			string_builder_append(&sb, uint64_t(argc));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));

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
		case OpCode::Ret: {
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
			string_builder_append(&sb, SV("[DEBUG] | return_value.u32 = "));
			string_builder_append(&sb, uint64_t(return_value.u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));

			debug_print_stack(ctx, sp, bp);			
			break;
		}
		case OpCode::ConditionalJump: {
			uint32_t jump_target = bytecode_read_scalar<uint32_t>(ctx, mp, &ip);
			Value condition = pop(ctx, &sp);
			if (condition.u32) {
				ip = jump_target;
			}
			break;
		}
		case OpCode::Jump: {
			uint32_t jump_target = bytecode_read_scalar<uint32_t>(ctx, mp, &ip);
			ip = jump_target;
			break;
		}
		case OpCode::StoreArg: {
			uint8_t i_arg = bytecode_read_scalar<uint8_t>(ctx, mp, &ip);

			// debug print
			string_builder_append(&sb, SV("[DEBUG] | i_arg = "));
			string_builder_append(&sb, uint64_t(i_arg));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));

			// Set the arg to the top of the stack
			uint32_t local_address = bp - ctx->callstack_argc[cp] + i_arg + 1;
			ctx->stack[local_address] = pop(ctx, &sp);
			break;
		}
		case OpCode::LoadArg: {
			uint8_t i_arg = bytecode_read_scalar<uint8_t>(ctx, mp, &ip);
			// Push the arg on top of the stack
			uint32_t local_address = bp - ctx->callstack_argc[cp] + i_arg + 1;
			push(ctx, &sp, ctx->stack[local_address]);
			
			// debug print
			string_builder_append(&sb, SV("[DEBUG] | i_arg = "));
			string_builder_append(&sb, uint64_t(i_arg));
			string_builder_append(&sb, SV(", pushed.u32 = "));
			string_builder_append(&sb, uint64_t(ctx->stack[local_address].u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode::StoreLocal: {
			uint8_t i_local = bytecode_read_scalar<uint8_t>(ctx, mp, &ip);
			// Set the local to the top of the stack
			uint32_t local_address = bp + i_local + 1;
			ctx->stack[local_address] = pop(ctx, &sp);

			// debug print
			string_builder_append(&sb, SV("[DEBUG] | i_local = "));
			string_builder_append(&sb, uint64_t(i_local));
			string_builder_append(&sb, SV(", popped.u32 = "));
			string_builder_append(&sb, uint64_t(ctx->stack[local_address].u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode::LoadLocal: {
			uint8_t i_local = bytecode_read_scalar<uint8_t>(ctx, mp, &ip);
			// Push the local on top of the stack
			uint32_t local_address = bp + i_local + 1;
			push(ctx, &sp, ctx->stack[local_address]);

			// debug print
			string_builder_append(&sb, SV("[DEBUG] | i_local = "));
			string_builder_append(&sb, uint64_t(i_local));
			string_builder_append(&sb, SV(", pushed.u32 = "));
			string_builder_append(&sb, uint64_t(ctx->stack[local_address].u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode::Load32: {
			Value ptr = pop(ctx, &sp);
			
			// debug print
			string_builder_append(&sb, SV("[DEBUG] | ptr = "));
			string_builder_append(&sb, uint64_t(ptr.u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));			
			break;
		}
		case OpCode::Store32: {
			Value operands[2] = {};
			pop_n(ctx, &sp, operands, 2);

			// debug print
			string_builder_append(&sb, SV("[DEBUG] | ptr = "));
			string_builder_append(&sb, uint64_t(operands[1].u32));
			string_builder_append(&sb, SV(", val.u32 = "));
			string_builder_append(&sb, uint64_t(operands[0].u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode::AddI32: {
			Value operands[2] = {};
			pop_n(ctx, &sp, operands, 2);
			Value result = {};
			result.i32 = operands[1].i32 + operands[0].i32;
			push(ctx, &sp, result);

			// debug print
			string_builder_append(&sb, SV("[DEBUG] | arg0 = "));
			string_builder_append(&sb, uint64_t(operands[1].u32));
			string_builder_append(&sb, SV(" + arg1 = "));
			string_builder_append(&sb, uint64_t(operands[0].u32));
			string_builder_append(&sb, SV(" => "));
			string_builder_append(&sb, uint64_t(result.u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode::SubI32: {
			Value operands[2] = {};
			pop_n(ctx, &sp, operands, 2);
			Value result = {};
			result.i32 = operands[1].i32 - operands[0].i32;
			push(ctx, &sp, result);

			// debug print
			string_builder_append(&sb, SV("[DEBUG] | arg0 = "));
			string_builder_append(&sb, uint64_t(operands[1].u32));
			string_builder_append(&sb, SV(" + arg1 = "));
			string_builder_append(&sb, uint64_t(operands[0].u32));
			string_builder_append(&sb, SV(" => "));
			string_builder_append(&sb, uint64_t(result.u32));
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode::LteI32: {
			Value operands[2] = {};
			pop_n(ctx, &sp, operands, 2);
			Value result = {};
			result.i32 = operands[1].i32 <= operands[0].i32;
			push(ctx, &sp, result);
			break;
		}
		case OpCode::DebugLabel: {
			char debug_buffer[64] = {};
			uint32_t string_length = bytecode_read_scalar<uint32_t>(ctx, mp, &ip);
			for (uint32_t i = 0; i < string_length && i < 64; ++i) {
				debug_buffer[i] = bytecode_read_scalar<char>(ctx, mp, &ip);
			}
			
			// debug print
			string_builder_append(&sb, SV("[DEBUG] "));
			string_builder_append(&sb, sv{debug_buffer, string_length});
			string_builder_append(&sb, '\n');
			cross::log(cross::stderr, string_builder_get_string(&sb));
			break;
		}
		case OpCode::Count: {
			break;
		}
		}
	}
}

#include "debug.h"
#include "parser.h"
#include "opcodes.h"
#include "cross.h"

void print_bytecode(const uint8_t *bytecode, uint32_t bytecode_length)
{
	char logbuf[64] = {0};
	
	for (uint32_t offset = 0; offset < bytecode_length; ++offset) {
		uint8_t opcode = bytecode[offset];
		if (opcode >= (uint8_t)OpCode_Count) {
			break;
		}

		OpCode opcode_kind = (OpCode)opcode;

		StringBuilder sb = string_builder_from_buffer(logbuf, sizeof(logbuf));
		string_builder_append_u64(&sb, (uint64_t)(offset));
		string_builder_append_char(&sb, '\t');
		string_builder_append_sv(&sb, SV(OpCode_str[(uint8_t)(opcode_kind)]));		
		string_builder_append_char(&sb, ' ');

#define PRINT_BYTE                                                                                                     \
	{                                                                                                                  \
		const uint8_t *bytecode_u8 = (const uint8_t*)(bytecode + offset + 1);                         \
		string_builder_append_u64(&sb, (uint64_t)(bytecode_u8[0]));                                                                        \
		offset += sizeof(uint8_t);                                                                                     \
	}
#define PRINT_U32                                                                                                      \
	{                                                                                                                  \
		const uint32_t *bytecode_u32 = (const uint32_t*)(bytecode + offset + 1);                      \
		string_builder_append_u64(&sb, (uint64_t)(bytecode_u32[0]));                                                                        \
		offset += sizeof(uint32_t);                                                                                    \
	}
#define PRINT_F32                                                                                                      \
	{                                                                                                                  \
		const float *bytecode_f32 = (const float*)(bytecode + offset + 1);                      \
		string_builder_append_f32(&sb, bytecode_f32[0]);                                                                        \
		offset += sizeof(float);                                                                                    \
	}		
#define PRINT_TYPEID PRINT_U32

		switch (opcode_kind) {
		case OpCode_Halt: break;
		case OpCode_Nop: break;
		case OpCode_PushU32:
			PRINT_U32;
		break;
		case OpCode_PushF32:
			PRINT_F32;
		break;
		case OpCode_PushStr: {
			const uint32_t *bytecode_u32 = (const uint32_t*)(bytecode + offset + 1);
			const uint32_t string_index = bytecode_u32[0];
			string_builder_append_char(&sb, '#');
			string_builder_append_u64(&sb, (uint64_t)string_index);
			offset += sizeof(uint32_t);
			
			break;
		}
		case OpCode_Pop:
		case OpCode_Swap: {
			break;
		}
		case OpCode_Call:
			PRINT_U32;
		string_builder_append_char(&sb, ' ');
			PRINT_BYTE;
			break;
		case OpCode_CallInModule:
			PRINT_BYTE;
			string_builder_append_char(&sb, ' ');
			PRINT_BYTE;
			string_builder_append_char(&sb, ' ');
			PRINT_BYTE;
			break;
		case OpCode_CallForeign:
			PRINT_BYTE;
			string_builder_append_char(&sb, ' ');
			PRINT_BYTE;
			break;
		case OpCode_Ret:
			break;
		case OpCode_ConditionalJump:
		case OpCode_Jump:
			PRINT_U32
			break;
		case OpCode_StoreArg:
		case OpCode_LoadArg:
		case OpCode_StoreLocal:
		case OpCode_LoadLocal:
			PRINT_BYTE;
			break;
		case OpCode_Load8:
		case OpCode_Load16:
		case OpCode_Load32:
		case OpCode_Load64:
		case OpCode_Store8:
		case OpCode_Store16:
		case OpCode_Store32:
		case OpCode_SliceData:
		case OpCode_SliceLength:
		case OpCode_MulI32:
		case OpCode_AddU8:
		case OpCode_AddU16:
		case OpCode_AddU32:
		case OpCode_SubU8:
		case OpCode_SubU16:
		case OpCode_SubU32:
		case OpCode_LteI32:
		case OpCode_LtI32:
		case OpCode_GteI32:
		case OpCode_EqI32:
		case OpCode_And:
			break;
		case OpCode_DebugLabel: {
			const uint32_t *bytecode_u32 = (const uint32_t*)(bytecode + offset + 1);
			sv debug_label = {0};
			debug_label.length = bytecode_u32[0];
			offset += sizeof(uint32_t);
			debug_label.chars = (const char*)(bytecode_u32 + 1);
			offset += debug_label.length;

			string_builder_append_sv(&sb, debug_label);
			break;
		}
		case OpCode_Count:
			break;
		}

		string_builder_append_char(&sb, '\n');
		cross_log(cross_stderr, string_builder_get_string(&sb));
	}
}

void build_error_at(sv code, span error, StringBuilder *sb)
{
	sv error_str = sv_substr(code, error);
	uint32_t line = 1;
	for (uint32_t i = 0; i < error.start; ++i) {
		if (code.chars[i] == '\n') {
			line += 1;
		}
	}
	// Error at: '<errorstr>'
	string_builder_append_sv(sb, SV("Error at line "));
	string_builder_append_u64(sb, line);
	string_builder_append_sv(sb, SV(": '"));
	string_builder_append_sv(sb, error_str);
	string_builder_append_sv(sb, SV("'\n"));
}

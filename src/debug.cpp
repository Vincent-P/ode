#include "debug.h"
#include "parser.h"
#include "opcodes.h"
#include "cross.h"

static void print_ast_rec(
	sv input, slice<const Token> tokens, slice<const AstNode> nodes, uint32_t node_index, int indent)
{
	const AstNode *node = nodes.elements + node_index;
	if (node->left_child_index != INVALID_NODE_INDEX) {
		// putchar('(');
	}

	if (node->atom_token_index != INVALID_NODE_INDEX) {
		// const Token *token = vec_at(tokens, node->atom_token_index);
		// sv token_str = sv_substr(input, token->span);
		// fprintf(stdout, "%.*s", int(token_str.length), token_str.chars);
	}

	if (node->left_child_index != INVALID_NODE_INDEX) {
		const AstNode *child = nodes.elements + node->left_child_index;
		print_ast_rec(input, tokens, nodes, node->left_child_index, indent);

		while (child->right_sibling_index != INVALID_NODE_INDEX) {
			uint32_t next_child_index = child->right_sibling_index;
			// putchar('\n');
			// print_indent(indent + 1);
			print_ast_rec(input, tokens, nodes, next_child_index, indent + 1);
			child = nodes.elements + next_child_index;
		}

		// putchar(')');
	}
}

void print_ast(sv input, slice<const Token> tokens, slice<const AstNode> nodes, uint32_t root_index)
{
	const AstNode *root = nodes.elements + root_index;
	if (root_index >= nodes.length || root->left_child_index == INVALID_NODE_INDEX) {
		return;
	}

	const AstNode *child = nodes.elements + root->left_child_index;
	print_ast_rec(input, tokens, nodes, root->left_child_index, 0);
	while (child->right_sibling_index != INVALID_NODE_INDEX) {
		// putchar('\n');
		print_ast_rec(input, tokens, nodes, child->right_sibling_index, 0);
		child = nodes.elements + child->right_sibling_index;
	}
	// putchar('\n');
}

void print_bytecode(const uint8_t *bytecode, uint32_t bytecode_length)
{
	char logbuf[64] = {};
	
	for (uint32_t offset = 0; offset < bytecode_length; ++offset) {
		uint8_t opcode = bytecode[offset];
		if (opcode >= uint8_t(OpCode::Count)) {
			break;
		}

		OpCode opcode_kind = OpCode(opcode);

		StringBuilder sb = string_builder_from_buffer(logbuf);
		string_builder_append(&sb, uint64_t(offset));
		string_builder_append(&sb, '\t');
		string_builder_append(&sb, SV(OpCode_str[uint8_t(opcode_kind)]));		
		string_builder_append(&sb, ' ');

#define PRINT_BYTE                                                                                                     \
	{                                                                                                                  \
		const uint8_t *bytecode_u8 = reinterpret_cast<const uint8_t *>(bytecode + offset + 1);                         \
		string_builder_append(&sb, uint64_t(bytecode_u8[0]));                                                                        \
		offset += sizeof(uint8_t);                                                                                     \
	}
#define PRINT_U32                                                                                                      \
	{                                                                                                                  \
		const uint32_t *bytecode_u32 = reinterpret_cast<const uint32_t *>(bytecode + offset + 1);                      \
		string_builder_append(&sb, uint64_t(bytecode_u32[0]));                                                                        \
		offset += sizeof(uint32_t);                                                                                    \
	}
#define PRINT_TYPEID PRINT_U32

		switch (opcode_kind) {
		case OpCode::Halt: break;
		case OpCode::Nop: break;
		case OpCode::PushU32:
			PRINT_U32;
			break;
		case OpCode::Call:
			PRINT_BYTE;
			PRINT_BYTE;
			break;
		case OpCode::CallInModule:
			PRINT_BYTE;
			PRINT_BYTE;
			PRINT_BYTE;
			break;
		case OpCode::Ret:
			break;
		case OpCode::ConditionalJump:
		case OpCode::Jump:
			PRINT_U32
			break;
		case OpCode::StoreArg:
		case OpCode::LoadArg:
		case OpCode::StoreLocal:
		case OpCode::LoadLocal:
			PRINT_BYTE;
			break;
		case OpCode::Load32:
		case OpCode::Store32:
		case OpCode::AddI32:
		case OpCode::SubI32:
		case OpCode::LteI32:
			break;
		case OpCode::DebugLabel: {
			const uint32_t *bytecode_u32 = reinterpret_cast<const uint32_t *>(bytecode + offset + 1);
			sv debug_label = {};
			debug_label.length = bytecode_u32[0];
			offset += sizeof(uint32_t);
			debug_label.chars = reinterpret_cast<const char *>(bytecode_u32 + 1);
			offset += debug_label.length;

			string_builder_append(&sb, debug_label);
			break;
		}
		case OpCode::Count:
			break;
		}

		string_builder_append(&sb, '\n');
		cross::log(cross::stderr, string_builder_get_string(&sb));
	}
}

#include "debug.h"
#include "image.h"
#include "parser.h"

#include <stdio.h>

static void print_indent(int indent)
{
	for (int i = 0; i < indent; ++i) {
		putchar(' ');
		putchar(' ');
	}
}

static void print_ast_rec(
	sv input, const vec<Token> *tokens, const vec<AstNode> *nodes, uint32_t node_index, int indent)
{
	const AstNode *node = vec_at(nodes, node_index);
	if (node->left_child_index != INVALID_NODE_INDEX) {
		putchar('(');
	}

	if (node->atom_token_index != INVALID_NODE_INDEX) {
		const Token *token = vec_at(tokens, node->atom_token_index);
		sv token_str = sv_substr(input, token->span);
		fprintf(stdout, "%.*s", int(token_str.length), token_str.chars);
	}

	if (node->left_child_index != INVALID_NODE_INDEX) {
		const AstNode *child = vec_at(nodes, node->left_child_index);
		print_ast_rec(input, tokens, nodes, node->left_child_index, indent);

		while (child->right_sibling_index != INVALID_NODE_INDEX) {
			uint32_t next_child_index = child->right_sibling_index;
			putchar('\n');
			print_indent(indent + 1);
			print_ast_rec(input, tokens, nodes, next_child_index, indent + 1);
			child = vec_at(nodes, next_child_index);
		}

		putchar(')');
	}
}

void print_ast(sv input, const vec<Token> *tokens, const vec<AstNode> *nodes, uint32_t root_index)
{
	const AstNode *root = vec_at(nodes, root_index);
	if (root_index >= nodes->length || root->left_child_index == INVALID_NODE_INDEX) {
		return;
	}

	const AstNode *child = vec_at(nodes, root->left_child_index);
	print_ast_rec(input, tokens, nodes, root->left_child_index, 0);
	while (child->right_sibling_index != INVALID_NODE_INDEX) {
		putchar('\n');
		print_ast_rec(input, tokens, nodes, child->right_sibling_index, 0);
		child = vec_at(nodes, child->right_sibling_index);
	}
	putchar('\n');
}

void print_bytecode(const uint8_t *bytecode, uint32_t bytecode_length)
{
	for (uint32_t offset = 0; offset < bytecode_length; ++offset) {
		uint8_t opcode = bytecode[offset];
		if (opcode >= uint8_t(OpCodeKind::Count)) {
			break;
		}

		OpCodeKind opcode_kind = OpCodeKind(opcode);
		fprintf(stdout, "%u\t%s", offset, OpCode_str[uint8_t(opcode_kind)]);

#define PRINT_BYTE                                                                                                     \
	{                                                                                                                  \
		const uint8_t *bytecode_u8 = reinterpret_cast<const uint8_t *>(bytecode + offset + 1);                         \
		fprintf(stdout, " %u", bytecode_u8[0]);                                                                        \
		offset += sizeof(uint8_t);                                                                                     \
	}
#define PRINT_U32                                                                                                      \
	{                                                                                                                  \
		const uint32_t *bytecode_u32 = reinterpret_cast<const uint32_t *>(bytecode + offset + 1);                      \
		fprintf(stdout, " %u", bytecode_u32[0]);                                                                       \
		offset += sizeof(uint32_t);                                                                                    \
	}
#define PRINT_TYPEID PRINT_U32

		switch (opcode_kind) {
		case OpCodeKind::LoadConstantU32:
			PRINT_BYTE;
			break;
		case OpCodeKind::LoadConstantStr:
			PRINT_BYTE;
			break;
		case OpCodeKind::Call:
			PRINT_BYTE;
			break;
		case OpCodeKind::CallForeign:
			break;
		case OpCodeKind::Ret:
			break;
		case OpCodeKind::ConditionalJump:
		case OpCodeKind::Jump:
			PRINT_U32
			break;
		case OpCodeKind::Halt:
			break;
		case OpCodeKind::StoreLocal:
		case OpCodeKind::LoadLocal:
			PRINT_BYTE;
			break;
		case OpCodeKind::StackAlloc:
		case OpCodeKind::Load:
		case OpCodeKind::Store:
			PRINT_TYPEID;
			break;
		case OpCodeKind::IAdd:
			break;
		case OpCodeKind::ISub:
			break;
		case OpCodeKind::ILessThanEq:
			break;
		case OpCodeKind::PtrOffset:
			PRINT_TYPEID;
			break;
		case OpCodeKind::DebugLabel: {
			const uint32_t *bytecode_u32 = reinterpret_cast<const uint32_t *>(bytecode + offset + 1);
			uint32_t sv_length = bytecode_u32[0];
			offset += sizeof(uint32_t);

			const char *bytecode_chars = reinterpret_cast<const char *>(bytecode_u32 + 1);
			char buffer[128] = {};
			for (uint32_t i = 0; i < sv_length && i < 127; ++i) {
				buffer[i] = bytecode_chars[i];
			}
			offset += sv_length;

			fprintf(stdout, " %s", buffer);
			break;
		}
		case OpCodeKind::Count:
			break;
		}

		fprintf(stdout, "\n");
	}
}

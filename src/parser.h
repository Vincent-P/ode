#pragma once
#include "core/core.h"
#include "constants.h"
#include "lexer.h"
#include "type_id.h"

struct CompilationUnit;

// Generic LISP tree node
typedef struct AstNode
{
	span span;
	uint32_t child_count;
	// Not null if atom
	uint32_t atom_token_index;
	// Not null if s-expr
	uint32_t left_child_index;
	uint32_t right_sibling_index;
} AstNode;

typedef struct Parser
{
	// input
	CompilationUnit* compunit;
	// parsing data
	uint32_t i_current_token;
	// error handling
	TokenKind expected_token_kind;
} Parser;

// Invalid index used for both atom_token_index and node indices
const uint32_t INVALID_NODE_INDEX = ~0u;

void parse_module(Parser *parser);








// Helpers for AstNode
inline bool ast_has_right_sibling(const AstNode *node)
{
	return node->right_sibling_index != INVALID_NODE_INDEX;
}
	
inline bool ast_has_left_child(const AstNode *node)
{
	return node->left_child_index != INVALID_NODE_INDEX;
}
	
inline bool ast_is_valid(uint32_t node_index)
{
	return node_index != INVALID_NODE_INDEX;
}
	
inline bool ast_is_atom(const AstNode *node)
{
	return node->atom_token_index != INVALID_NODE_INDEX;
}
	
const AstNode *ast_get_node(const CompilationUnit *compunit, uint32_t node_index);
const AstNode *ast_get_left_child(const CompilationUnit *compunit, const AstNode *node);
const AstNode *ast_get_right_sibling(const CompilationUnit *compunit, const AstNode *node);
const Token *ast_get_token(const CompilationUnit *compunit, const AstNode *node);

// Typed nodes

typedef struct DefineNode
{
	const Token *function_name_token;
	Token arg_identifiers[MAX_ARGUMENTS];
	const AstNode *arg_nodes[MAX_ARGUMENTS];
	const AstNode *return_type_node;
	const AstNode *body_node;
	uint32_t args_length;
	// internal
	const AstNode *arglist_node;
} DefineNode;

typedef struct StructNode
{
	const Token *struct_name_token;
	Token field_identifiers[MAX_STRUCT_FIELDS];
	const AstNode *field_type_nodes[MAX_STRUCT_FIELDS];
	uint32_t fields_length;
} StructNode;

typedef struct IfNode
{
	const AstNode *cond_expr_node;
	const AstNode *then_expr_node;
	const AstNode *else_expr_node;
} IfNode;

typedef struct LetNode
{
	const Token *name_token;
	const AstNode *value_node;
} LetNode;

typedef struct BinaryOpNode
{
	const AstNode *lhs_node;
	const AstNode *rhs_node;
} BinaryOpNode;

typedef struct UnaryOpNode
{
	const AstNode *value_node;
} UnaryOpNode;

typedef struct FieldNode
{
	const AstNode *expr_node;
	const Token *field_token;
} FieldNode;

typedef struct PtrOffsetNodes
{
	const AstNode * return_type_node;
	const AstNode *base_pointer_node;
	const AstNode *offset_node;
} PtrOffsetNodes;

TypeID parse_type(CompilationUnit *, CompilerModule*, const AstNode *node);
void parse_define_sig(CompilationUnit*, const AstNode *node, DefineNode *output);
void parse_define_body(CompilationUnit*, const AstNode *node, DefineNode *output);
void parse_struct(CompilationUnit*, const AstNode *node, StructNode *output);
void parse_if(CompilationUnit*, const AstNode *node, IfNode *output);
void parse_let(CompilationUnit*, const AstNode *node, LetNode *output);
void parse_field(CompilationUnit*, const AstNode *node, FieldNode *output);
void parse_ptr_offset(CompilationUnit*, const AstNode *node, PtrOffsetNodes *output);
void parse_nary_op(CompilationUnit*, const AstNode*, uint32_t n, const AstNode **out_nodes);

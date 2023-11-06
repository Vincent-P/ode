#pragma once
#include "core.h"
#include "constants.h"
#include "lexer.h"
#include "type_id.h"

struct CompilationUnit;

// Generic LISP tree node
struct AstNode
{
	span span;
	uint32_t child_count;
	// Not null if atom
	uint32_t atom_token_index;
	// Not null if s-expr
	uint32_t left_child_index;
	uint32_t right_sibling_index;
};

struct Parser
{
	// input
	CompilationUnit* compunit;
	// parsing data
	uint32_t i_current_token;
	// error handling
	TokenKind expected_token_kind;
};

// Invalid index used for both atom_token_index and node indices
static constexpr uint32_t INVALID_NODE_INDEX = ~uint32_t(0);

vec<AstNode> parse_module(Parser *parser);

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
	
inline const AstNode *ast_get_node(const vec<AstNode> *ast, uint32_t node_index)
{
	return vec_at(ast, node_index);
}
	
inline const AstNode *ast_get_left_child(const vec<AstNode> *ast, const AstNode *node)
{
	return vec_at(ast, node->left_child_index);
}
	
inline const AstNode *ast_get_right_sibling(const vec<AstNode> *ast, const AstNode *node)
{
	return vec_at(ast, node->right_sibling_index);
}
	
inline const Token *ast_get_token(const vec<Token> *tokens, const AstNode *node)
{
	return vec_at(tokens, node->atom_token_index);
}

// Typed nodes

struct DefineNode
{
	const Token *function_name_token;
	Token arg_identifiers[MAX_ARGUMENTS];
	const AstNode *arg_nodes[MAX_ARGUMENTS];
	const AstNode *return_type_node;
	const AstNode *body_node;
	uint32_t args_length;
	// internal
	const AstNode *arglist_node;
};

struct StructNode
{
	const Token *struct_name_token;
	Token field_identifiers[MAX_STRUCT_FIELDS];
	const AstNode *field_type_nodes[MAX_STRUCT_FIELDS];
	uint32_t fields_length;
};

struct IfNode
{
	const AstNode *cond_expr_node;
	const AstNode *then_expr_node;
	const AstNode *else_expr_node;
};

struct LetNode
{
	const Token *name_token;
	const AstNode *value_node;
};

struct BinaryOpNode
{
	const AstNode *lhs_node;
	const AstNode *rhs_node;
};

struct UnaryOpNode
{
	const AstNode *value_node;
};

struct FieldNode
{
	const AstNode *expr_node;
	const Token *field_token;
};

struct PtrOffsetNodes
{
	const AstNode * return_type_node;
	const AstNode *base_pointer_node;
	const AstNode *offset_node;
};

TypeID parse_type(CompilationUnit *, CompilerModule*, const AstNode *node);
void parse_define_sig(CompilationUnit*, const AstNode *node, DefineNode *output);
void parse_define_body(CompilationUnit*, const AstNode *node, DefineNode *output);
void parse_struct(CompilationUnit*, const AstNode *node, StructNode *output);
void parse_if(CompilationUnit*, const AstNode *node, IfNode *output);
void parse_let(CompilationUnit*, const AstNode *node, LetNode *output);
void parse_binary_op(CompilationUnit*, const AstNode *node, BinaryOpNode *output);
void parse_unary_op(CompilationUnit*, const AstNode *node, UnaryOpNode *output);
void parse_field(CompilationUnit*, const AstNode *node, FieldNode *output);
void parse_ptr_offset(CompilationUnit*, const AstNode *node, PtrOffsetNodes *output);

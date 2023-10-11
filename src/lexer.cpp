#include "lexer.h"
#include <stdio.h>

static bool is_identifier_char(char c)
{
	bool is_lower = 'a' <= c && c <= 'z';
	bool is_upper = 'A' <= c && c <= 'Z';
	bool is_number = '0' <= c && c <= '9';
	bool is_operator = c == '+' || c == '-' || c == '/' || c == '*' || c == '<' || c == '>' || c == '=';
	return c == '_' || is_operator || is_lower || is_upper || is_number;
}

// Reserve the identifier that start with a number for literals
static bool is_identifier_first_char(char c)
{
	bool is_number = '0' <= c && c <= '9';
	return is_identifier_char(c) && !is_number;
}

static bool is_whitespace(char c)
{
	return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}

void lexer_scan(Lexer *lexer, sv input)
{
	if (lexer->result != LexerResult::Ok) {
		return;
	}

	uint32_t input_offset = 0;
	while (true) {
		// Eat all whitespace
		for (; input_offset < input.length; ++input_offset) {
			if (!is_whitespace(input.chars[input_offset])) {
				break;
			}
		}

		// End of input
		if (input_offset >= input.length) {
			lexer->result = LexerResult::LexerDone;
			break;
		}

		Token token = {};
		token.span.start = input_offset;
		uint32_t token_length = 1;

		const char first_char = input.chars[input_offset];
		if (first_char == '(') {
			token.kind = TokenKind::LeftParen;
		} else if (first_char == ')') {
			token.kind = TokenKind::RightParen;
		} else if ('0' <= first_char && first_char <= '9') {
			char next_char = input.chars[input_offset + token_length];
			while (token_length < input.length && '0' <= next_char && next_char <= '9') {
				token_length += 1;
				next_char = input.chars[input_offset + token_length];
			}
			token.kind = TokenKind::Number;
		} else if (is_identifier_first_char(first_char)) {
			while (token_length < input.length && is_identifier_char(input.chars[input_offset + token_length])) {
				token_length += 1;
			}
			token.kind = TokenKind::Identifier;
		} else if (first_char == '"') {
			while (token_length < input.length && input.chars[input_offset + token_length] != '"') {
				token_length += 1;
			}
			// TODO: Handle string to eof error
			if (token_length >= input.length) {
			}
			// Eat the ending double-quote
			token_length += 1;
			token.kind = TokenKind::StringLiteral;
		} else {
			lexer->result = LexerResult::LexerUnknownToken;
			lexer->error = span{input_offset, input_offset + 1};
			break;
		}
		token.span.end = token.span.start + token_length;
		input_offset += token_length;

		// Add the token to our list
		vec_append(&lexer->tokens, token);
	}
}

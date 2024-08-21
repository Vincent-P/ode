#include "lexer.h"
#include "compiler.h"

static bool is_number(char c)
{
	return '0' <= c && c <= '9';
}

static bool is_identifier_char(char c)
{
	bool is_lower = 'a' <= c && c <= 'z';
	bool is_upper = 'A' <= c && c <= 'Z';
	bool is_operator = c == '+' || c == '-' || c == '/' || c == '*' || c == '<' || c == '>' || c == '=';
	bool is_slice = c == '[' || c == ']';
	return c == '_' || is_operator || is_lower || is_upper || is_number(c) || is_slice;
}

// Reserve the identifier that start with a number for literals
static bool is_identifier_first_char(char c)
{
	return is_identifier_char(c) && !is_number(c);
}

static bool is_whitespace(char c)
{
	return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}

static LexerResult lexer_scan(Arena *arena, StringPool *string_pool, sv input)
{	
	LexerResult result = {0};
	result.tokens = array_init(arena);

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
			break;
		}

		Token token = {0};
		token.span.start = input_offset;
		uint32_t token_length = 1;

		const char first_char = input.chars[input_offset];
		char lookahead_char = 0;
		if (input_offset + 1 < input.length) {
			lookahead_char = input.chars[input_offset + 1];
		}
		
		if (first_char == '(') {
			token.kind = TokenKind_LeftParen;
		} else if (first_char == ')') {
			token.kind = TokenKind_RightParen;
		} else if (first_char == '-' && is_number(lookahead_char)) {
			// negative number
			token_length += 1; // eat the minus sign
			uint64_t parsed_number = (unsigned char)lookahead_char - (unsigned char)'0';
			char next_char = input.chars[input_offset + token_length];
			while (token_length < input.length && is_number(next_char)) {
				uint8_t digit = (unsigned char)next_char - (unsigned char)'0';
				parsed_number = parsed_number * 10 + digit;
				token_length += 1;
				next_char = input.chars[input_offset + token_length];
			}
			// floating point
			if (token_length < input.length && next_char == '.') {
				float parsed_float = (float)parsed_number;
				float divisor = 1.0f;
				uint32_t parsed_decimals = 0;
				token_length += 1;
				next_char = input.chars[input_offset + token_length];
				while (token_length < input.length && is_number(next_char)) {
					uint8_t digit = (unsigned char)next_char - (unsigned char)'0';
					parsed_decimals = parsed_decimals * 10 + digit;
					divisor = divisor / 10.0f;
					token_length += 1;
					next_char = input.chars[input_offset + token_length];
				}
				parsed_float = parsed_float + ((float)parsed_decimals * divisor);
				parsed_float = -parsed_float;

				token.kind = TokenKind_FloatingNumber;
				token.data.f32 = parsed_float;
			} else {
				token.kind = TokenKind_SignedNumber;
				token.data.i32 = -(int32_t)(uint32_t)parsed_number;
			}
		} else if (is_number(first_char)) {
			// positive number
			uint64_t parsed_number = (unsigned char)first_char - (unsigned char)'0';
			char next_char = input.chars[input_offset + token_length];
			while (token_length < input.length && is_number(next_char)) {
				uint8_t digit = (unsigned char)next_char - (unsigned char)'0';
				parsed_number = parsed_number * 10 + digit;
				token_length += 1;
				next_char = input.chars[input_offset + token_length];
			}
			// floating point
			if (token_length < input.length && next_char == '.') {
				float parsed_float = (float)parsed_number;
	

				float divisor = 1.0f;
				uint32_t parsed_decimals = 0;
				token_length += 1;
				next_char = input.chars[input_offset + token_length];
				while (token_length < input.length && is_number(next_char)) {
					uint8_t digit = (unsigned char)next_char - (unsigned char)'0';
					parsed_decimals = parsed_decimals * 10 + digit;
					divisor = divisor / 10.0f;
					token_length += 1;
					next_char = input.chars[input_offset + token_length];
				}
				parsed_float = parsed_float + ((float)parsed_decimals * divisor);

				token.kind = TokenKind_FloatingNumber;
				token.data.f32 = parsed_float;
			} else {
				// It does not have a floating point delimiter, it is a positive number
				token.kind = TokenKind_UnsignedNumber;
				token.data.u32 = (uint32_t)parsed_number;
			}
		} else if (is_identifier_first_char(first_char)) {
			while (token_length < input.length && is_identifier_char(input.chars[input_offset + token_length])) {
				token_length += 1;
			}
			token.kind = TokenKind_Identifier;

			sv identifier_str = {input.chars + token.span.start, token_length};
			token.data.sid = string_pool_intern(string_pool, identifier_str);
		} else if (first_char == '"') {

			// Write the string literal to the string buffer
			uint32_t escaped_string_size = 0;
			sv string_str = {0};
			string_str.chars = input.chars + input_offset + token_length;
			while (token_length < input.length && input.chars[input_offset + token_length] != '"') {
				escaped_string_size += 1;
				token_length += 1;
			}
			string_str.length = escaped_string_size;

			ASSERT(token_length < input.length); // TODO: Handle string to eof error
			// Eat the ending double-quote
			token_length += 1;

			token.kind = TokenKind_StringLiteral;
			token.data.sid = string_pool_intern(string_pool, string_str);
		} else {
			result.error_span = (span){input_offset, input_offset + 1};
			break;
		}

		token.span.end = token.span.start + token_length;
		input_offset += token_length;

		// Append to output
		array_push(arena, result.tokens, token);
	}

	result.success = true;
	return result;
}

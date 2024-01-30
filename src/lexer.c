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

void lexer_scan(CompilationUnit *compunit)
{
	if (compunit->error.code != ErrorCode_Ok) {
		return;
	}

	Error *error = &compunit->error;
	sv input = compunit->input;

	uint32_t token_number_size = 0;
	uint32_t token_string_size = 0; // Number of string token
	uint32_t string_buffer_offset = 0; // Current offset into the string buffer
	
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
			error->code = ErrorCode_Ok;
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
			token.kind = TokenKind_SignedNumber;
			// Add the number literal to the compilation unit data
			if (token_number_size >= ARRAY_LENGTH(compunit->token_signed_numbers)) {
				__debugbreak();
			}
			token.data = token_number_size;
			compunit->token_signed_numbers[token.data] = -(int32_t)(uint32_t)parsed_number;
			token_number_size += 1;
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
			bool has_unsigned_suffix = token_length < input.length && next_char == 'u';
			
			token.kind = TokenKind_UnsignedNumber;
			// Add the number literal to the compilation unit data
			if (token_number_size >= ARRAY_LENGTH(compunit->token_unsigned_numbers)) {
				__debugbreak();
			}
			token.data = token_number_size;
			compunit->token_unsigned_numbers[token.data] = (uint32_t)parsed_number;
			token_number_size += 1;
		} else if (is_identifier_first_char(first_char)) {
			while (token_length < input.length && is_identifier_char(input.chars[input_offset + token_length])) {
				token_length += 1;
			}
			token.kind = TokenKind_Identifier;
		} else if (first_char == '"') {

			if (token_string_size >= ARRAY_LENGTH(compunit->token_strings_offset)) {
				// maximum string count reached.
				__debugbreak();
			}

			// Write the string literal to the string buffer
			uint32_t escaped_string_size = 0;
			compunit->token_strings_offset[token_string_size] = string_buffer_offset;

			while (token_length < input.length && input.chars[input_offset + token_length] != '"') {

				// TODO: Handle escape sequences
				if (string_buffer_offset + escaped_string_size >= ARRAY_LENGTH(compunit->token_string_buffer)) {
					// Crash if we overflow the string buffer
					__debugbreak();
				}
				compunit->token_string_buffer[string_buffer_offset + escaped_string_size] = input.chars[input_offset + token_length];
				escaped_string_size += 1;
				
				token_length += 1;
			}
			compunit->token_strings_length[token_string_size] = escaped_string_size;
			
			// TODO: Handle string to eof error
			if (token_length >= input.length) {
				__debugbreak();
			}
			// Eat the ending double-quote
			token_length += 1;
			token.data = token_string_size;
			token.kind = TokenKind_StringLiteral;
			token_string_size += 1;
		} else {
			error->code = ErrorCode_LexerUnknownToken;
			error->span = (span){input_offset, input_offset + 1};
			break;
		}
		token.span.end = token.span.start + token_length;
		input_offset += token_length;

		// Add the token to our list
		uint32_t i_token = compunit->tokens_length;
		compunit->tokens[i_token] = token;
		compunit->tokens_length += 1;
	}
}

#pragma once
#define _CRT_SECURE_NO_WARNINGS
#include <stdint.h>

#define ARRAY_LENGTH(x) sizeof(x) / sizeof(x[0])

// CRT stub
void *memset(void *dest, int c, size_t count);
void *memcpy(void *dest, const void *src, size_t count);
typedef _Bool bool;
enum Core_Constants
{
	false = 0,
	true = 1,
};
#define nullptr (void*)0

// Text span
struct span
{
	uint32_t start;
	uint32_t end;
};
typedef struct span span;

inline span span_extend(span a, span b)
{
	return (span){
		a.start < b.start ? a.start : b.start,
		a.end > b.end ? a.end : b.end,
	};
}

inline uint32_t span_length(span s)
{
	if (s.start <= s.end) {
		return s.end - s.start;
	} else {
		return 0;
	}
}

// String view
struct sv
{
	const char *chars;
	uint32_t length;
};
typedef struct sv sv;

#define SV(x) sv_from_null_terminated(x)
#define SV_LIT(x) {.chars = x, .length = sizeof(x)-1}

inline sv sv_from_null_terminated(const char *null_terminated)
{
	uint32_t size = 0;
	while (null_terminated[size] != '\0')
		size += 1;
	return (sv){null_terminated, size};
}

inline sv sv_substr(sv s, span span)
{
	return (sv){s.chars + span.start, span_length(span)};
}

inline bool sv_equals(sv a, sv b)
{
	if (a.length != b.length) {
		return false;
	}

	const char *a_chars = a.chars;
	const char *b_chars = b.chars;
	const char *a_end = a.chars + a.length;
	for (; a_chars < a_end;) {
		if (*a_chars != *b_chars) {
			return false;
		}
		a_chars += 1;
		b_chars += 1;
	}

	return true;
}

inline int32_t sv_to_int(sv string)
{
	int32_t n = 0;
	const char *cursor = string.chars;
	const char *end = cursor + string.length;
	while (cursor < end) {
		n = n * 10 + (int)(*cursor - '0');
		cursor += 1;
	}
	return n;
}

inline sv sv_offset(sv string, uint32_t offset)
{
	if (offset > string.length) {
		offset = string.length;
	}
	return (sv){string.chars + offset, string.length - offset};
}

// string builder
struct StringBuilder
{
	char *buffer;
	uint32_t size;
	uint32_t capacity;
};
typedef struct StringBuilder StringBuilder;

inline StringBuilder string_builder_from_buffer(char *buf, uint32_t bufsize)
{
	StringBuilder builder = {0};
	builder.buffer = buf;
	builder.size = 0;
	builder.capacity = bufsize;
	return builder;
}

inline void string_builder_append_sv(StringBuilder *builder, sv content)
{
	uint32_t size = builder->size;
	uint32_t cap = builder->capacity;
	uint32_t i = 0;
	for (; i < content.length && size + i < cap; ++i)
	{
		builder->buffer[size + i] = content.chars[i];
	}
	builder->size += i;
}

inline void string_builder_append_u64(StringBuilder *builder, uint64_t n)
{
	// largest power of 10 greater than n
	uint32_t divisor = 1;
	for (uint64_t d = n; d >= 10; d = d / 10) {
		divisor *= 10;
	}
	
	uint32_t size = builder->size;
	uint32_t cap = builder->capacity;
	uint32_t i = 0;
	for (; divisor != 0 && size + i < cap ; ++i)
	{
		uint64_t digit = n / divisor;
		builder->buffer[size + i] = '0' + (digit % 10);
		divisor = divisor / 10;
	}
	builder->size += i;
}

inline void string_builder_append_char(StringBuilder *builder, char c)
{
	uint32_t size = builder->size;
	uint32_t cap = builder->capacity;
	if (size < cap) {
		builder->buffer[size] = c;
		builder->size += 1;
	}
}

inline void string_builder_append_f32(StringBuilder *builder, float f)
{
	uint64_t integral = (uint64_t)(f);
	uint64_t decimals = (uint64_t)((f - (float)(integral)) * 1e9f);

	string_builder_append_u64(builder, integral);
	string_builder_append_char(builder, '.');
	string_builder_append_u64(builder, decimals);
}


inline sv string_builder_get_string(StringBuilder *builder)
{
	sv result = {0};
	result.chars = builder->buffer;
	result.length = builder->size;
	builder->size = 0; // Reset the size to 0 to chain `append` and `get_string`
	return result;
}

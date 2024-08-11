#pragma once

// String view

struct sv
{
	const char *chars;
	uint32_t length;
};
typedef struct sv sv;

#define SV(x) sv_from_null_terminated(x)
#define SV_LIT(x) {.chars = x, .length = sizeof(x)-1}

static sv sv_from_null_terminated(const char *null_terminated)
{
	uint32_t size = 0;
	while (null_terminated[size] != '\0')
		size += 1;
	return (sv){null_terminated, size};
}

static sv sv_substr(sv s, span span)
{
	return (sv){s.chars + span.start, span_length(span)};
}

static bool sv_equals(sv a, sv b)
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

static int32_t sv_to_int(sv string)
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

static sv sv_offset(sv string, uint32_t offset)
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

static StringBuilder string_builder_from_buffer(char *buf, uint32_t bufsize)
{
	StringBuilder builder = {0};
	builder.buffer = buf;
	builder.size = 0;
	builder.capacity = bufsize;
	return builder;
}

static void string_builder_append_sv(StringBuilder *builder, sv content)
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

static void string_builder_append_u64(StringBuilder *builder, uint64_t n)
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

static void string_builder_append_char(StringBuilder *builder, char c)
{
	uint32_t size = builder->size;
	uint32_t cap = builder->capacity;
	if (size < cap) {
		builder->buffer[size] = c;
		builder->size += 1;
	}
}

static void string_builder_append_f32(StringBuilder *builder, float f)
{
	uint64_t integral = (uint64_t)(f);
	uint64_t decimals = (uint64_t)((f - (float)(integral)) * 1e9f);

	string_builder_append_u64(builder, integral);
	string_builder_append_char(builder, '.');
	string_builder_append_u64(builder, decimals);
}


static sv string_builder_get_string(StringBuilder *builder)
{
	sv result = {0};
	result.chars = builder->buffer;
	result.length = builder->size;
	builder->size = 0; // Reset the size to 0 to chain `append` and `get_string`
	return result;
}

// string interning

struct StringPool
{
	char *string_buffer; // buffer containing the string data
	uint32_t *keys; // hashes of the strings
	uint32_t *sv_offset; // offset into the string_buffer
	uint32_t *sv_length; // length of the string into the string_buffer
	uint32_t string_buffer_length;
	uint32_t string_buffer_capacity;
	uint32_t capacity; // maximum number of elements (key + offset) that can be stored
	uint32_t length;
};
typedef struct StringPool StringPool;

struct StringId
{
	uint32_t id;
};
typedef struct StringId StringId;

static uint32_t sv_hash(sv value)
{
	uint32_t hash = 0x811c9dc5; // offset basis
	for (uint32_t i = 0; i < value.length; ++i)
	{
		hash = hash ^ (uint32_t)value.chars[i];
		hash = hash * 0x01000193; // 32 bit magic FNV-1a prime
	}
	ASSERT(hash != 0); // we use 0 as empty hash
	return hash;
}

static uint32_t string_pool_append_string(StringPool *pool, sv value)
{
	ASSERT(pool->string_buffer_length + value.length <= pool->string_buffer_capacity);
	uint32_t offset = pool->string_buffer_length;

	for (uint32_t i = 0; i < value.length; ++i)
		pool->string_buffer[offset + i] = value.chars[i];

	pool->string_buffer_length += value.length;
	return offset;
}

static StringId string_pool_intern(StringPool *pool, sv value)
{
	ASSERT(pool->length < pool->capacity);

	uint32_t key = sv_hash(value);
	uint32_t index = key % pool->capacity;
	while (pool->keys[index] != key && pool->keys[index] != 0)
		index = (index + 1) % pool->capacity;

	if (pool->keys[index] == 0)
	{
		pool->keys[index] = key;
		pool->sv_offset[index] = string_pool_append_string(pool, value);
		pool->sv_length[index] = value.length;
		pool->length += 1;
	}

	return (StringId){index};
}

static sv string_pool_get(StringPool *pool, StringId s)
{
	ASSERT(s.id < pool->capacity);
	sv result;
	result.chars = pool->string_buffer + pool->sv_offset[s.id];
	result.length = pool->sv_length[s.id];
	ASSERT(result.length > 0);
	return result;
}

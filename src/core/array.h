#pragma once
#include "./core.h"
#include "./arena.h"

struct ArrayHeader
{
	uint32_t count;
};

static void *array_init(Arena *memory)
{
	struct ArrayHeader *header = arena_alloc(memory, sizeof(struct ArrayHeader));
	*header = (struct ArrayHeader){0};
	return memory->begin;
}

static uint32_t array_count(const void *array)
{
	const struct ArrayHeader *header = (const struct ArrayHeader*)array - 1;
	return header->count;
}

#define array_check(array, i) (array_check_impl(array, i), array[i])
#define array_check_addr(array, i) (array_check_impl(array, i), &array[i])
#define array_push(memory, array, value) (array[array_push_impl(memory, array, sizeof(value))] = value)
#define array_push_addr(memory, array, value) (array[array_push_impl(memory, array, sizeof(value))] = value, &array[array_count(array) - 1])

static uint32_t array_push_impl(Arena *memory, void *array, uint32_t element_size)
{
	struct ArrayHeader *header = (struct ArrayHeader*)array - 1;
	ASSERT((byte*)array + header->count * element_size == memory->begin);
	header->count += 1;
	arena_alloc(memory, element_size);
	return header->count - 1;
}

static void array_check_impl(const void *array, uint32_t i)
{
	const struct ArrayHeader *header = (const struct ArrayHeader*)array - 1;
	ASSERT(i < header->count);
}

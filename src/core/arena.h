#pragma once
#include "./core.h"

typedef struct Arena
{
	uint8_t *begin;
	uint8_t *end;
} Arena;

inline void *arena_alloc(Arena *a, uint32_t size)
{
	uint32_t capacity = (uint32_t)(a->end - a->begin);
	ASSERT(1 <= capacity / size);
	uint32_t total = size;
	uint8_t *p = a->begin;
	a->begin += total;
	return memset(p, 0, total);
}

inline void *arena_alloc_n(Arena *a, uint32_t size, uint32_t count)
{
	uint32_t capacity = (uint32_t)(a->end - a->begin);
	ASSERT(count <= capacity / size);
	uint32_t total = size * count;
	uint8_t *p = a->begin;
	a->begin += total;
	return memset(p, 0, total);
}

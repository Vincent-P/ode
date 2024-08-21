#pragma once
#include "./core.h"

typedef struct Arena
{
	byte *begin;
	byte *end;
} Arena;

static void *arena_alloc(Arena *a, uint32_t size)
{
	uint32_t capacity = (uint32_t)(a->end - a->begin);
	ASSERT(1 <= capacity / size);
	uint32_t total = size;
	byte *p = a->begin;
	a->begin += total;
	return memset(p, 0, total);
}

static void *arena_alloc_n(Arena *a, uint32_t size, uint32_t count)
{
	uint32_t capacity = (uint32_t)(a->end - a->begin);
	ASSERT(count <= capacity / size);
	uint32_t total = size * count;
	byte *p = a->begin;
	a->begin += total;
	return memset(p, 0, total);
}

#pragma once
#include "core.h"

struct Arena
{
	uint8_t *begin;
	uint8_t *end;
};

void *alloc(Arena *a, uint32_t size, uint32_t count = 1)
{
	uint32_t capacity = a->end - a->begin;
	if (count > capacity / size) {
		__debugbreak();
	}
	uint32_t total = size * count;
	uint8_t *p = a->begin;
	a->begin += total;
	return memset(p, 0, total);
}

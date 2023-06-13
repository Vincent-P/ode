#pragma once
#include "core.h"

struct StackAllocator
{
	void *begin;
	void *end;
	void *current;
};

void stack_allocator_init(StackAllocator *allocator, void *buffer, size_t size);
void *stack_alloc(StackAllocator *allocator, size_t size);
void *stack_save_state(StackAllocator *allocator);
void stack_set_state(StackAllocator *allocator, void *state);

#include "stack_allocator.h"

void stack_allocator_init(StackAllocator *allocator, void *buffer, size_t size)
{
	allocator->begin = buffer;
	allocator->end = (uint8_t *)(buffer) + size;
	allocator->current = buffer;
}

void *stack_alloc(StackAllocator *allocator, size_t size)
{
	if ((uint8_t *)(allocator->current) + size >= allocator->end) {
		return nullptr;
	}

	void *p = allocator->current;
	allocator->current = (uint8_t *)(allocator->current) + size;
	return p;
}

void *stack_save_state(StackAllocator *allocator)
{
	return allocator->current;
}

void stack_set_state(StackAllocator *allocator, void *state)
{
	allocator->current = state;
}

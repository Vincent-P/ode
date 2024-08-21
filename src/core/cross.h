#pragma once
#include "./core.h"

void cross_init(void);

// -- io
uint64_t cross_get_file_last_write(sv filepath);

typedef struct ReadFileResult
{
	sv content;
	bool success;
} ReadFileResult;
ReadFileResult cross_read_entire_file(sv filepath);

bool cross_write_entire_file(sv filepath, byte *content, uint32_t content_size);
	
extern uint64_t cross_stdout;
extern uint64_t cross_stderr;
void cross_log(uint64_t handle, sv message);

// -- threads
void cross_sleep_ms(unsigned int ms);

// -- memory
void* cross_alloc(uint32_t size);

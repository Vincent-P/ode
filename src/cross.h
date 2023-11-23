#pragma once
#include "core.h"

namespace cross
{
extern uint64_t stdout;
extern uint64_t stderr;

void init();

// -- io
uint64_t get_file_last_write(const char *path, size_t path_length);

struct ReadFileResult
{
	sv content;
	bool success;
};
ReadFileResult read_entire_file(const char* filepath);
void log(uint64_t handle, sv message);

// -- threads
void sleep_ms(unsigned int ms);

} // namespace cross

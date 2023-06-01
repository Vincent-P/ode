#pragma once

namespace cross
{
void get_file_last_write(const char *path);
void sleep_ms(unsigned int ms);

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>

uint64_t get_file_last_write(const char *path, size_t path_length)
{
	wchar_t wide_path[256] = {};
	MultiByteToWideChar(CP_UTF8, MB_PRECOMPOSED, path, int(path_length), &wide_path[0], 256);

	HANDLE file =
		CreateFileW(wide_path, GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (file == INVALID_HANDLE_VALUE) {
		return 0;
	}

	FILETIME last_write_time = {};
	GetFileTime(file, nullptr, nullptr, &last_write_time);
	CloseHandle(file);

	ULARGE_INTEGER time;
	time.u.LowPart = last_write_time.dwLowDateTime;
	time.u.HighPart = last_write_time.dwHighDateTime;
	return time.QuadPart;
}

void sleep_ms(unsigned int ms)
{
	Sleep(ms);
}

#endif
} // namespace cross

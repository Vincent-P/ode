#include "./cross.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#else
#error "Unknown platform"
#endif

#if defined(_WIN32)
// -- io
uint64_t cross_stdout;
uint64_t cross_stderr;

void cross_init(void)
{
	cross_stdout = (uint64_t)(GetStdHandle(STD_OUTPUT_HANDLE));
	cross_stderr = (uint64_t)(GetStdHandle(STD_ERROR_HANDLE));
}

uint64_t cross_get_file_last_write(sv filepath)
{
	wchar_t wide_path[256] = {0};
	MultiByteToWideChar(CP_UTF8, MB_PRECOMPOSED, filepath.chars, (int)filepath.length, &wide_path[0], 256);

	HANDLE file =
		CreateFileW(wide_path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (file == INVALID_HANDLE_VALUE) {
		return 0;
	}

	FILETIME last_write_time = {0};
	GetFileTime(file, NULL, NULL, &last_write_time);
	CloseHandle(file);

	ULARGE_INTEGER time;
	time.u.LowPart = last_write_time.dwLowDateTime;
	time.u.HighPart = last_write_time.dwHighDateTime;
	return time.QuadPart;
}

ReadFileResult cross_read_entire_file(sv filepath)
{
	ReadFileResult result = {0};
	// Open file
        HANDLE file = CreateFileA(filepath.chars, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (file == INVALID_HANDLE_VALUE) {
		result.success = false;
                return result;
        }
	// Get size on disk
	uint32_t size = GetFileSize(file, NULL);
	// Allocate memory
        void *file_content = cross_alloc(size + 1);
	// Read file
	uint32_t bytes_read = 0;
	bool success = ReadFile(file, file_content, size, (LPDWORD)&bytes_read, NULL);
	if (!success) {
		CloseHandle(file);
		result.success = false;
		return result;
	}
	// Close
        ((char*)file_content)[size] = 0;	
        CloseHandle(file);
        result.content = (sv){(char*)file_content, size};
	result.success = true;
	return result;
}

void cross_log(uint64_t handle, sv message)
{
	WriteFile((HANDLE)handle, message.chars, message.length, NULL, NULL);
	if (message.length > 0 && message.chars[message.length] == 0) {
		// Make sure that we have a NULL terminator because OutputDebugString does not take a length
		OutputDebugStringA(message.chars);
	}
}


// -- threads

void cross_sleep_ms(unsigned int ms)
{
	Sleep(ms);
}

// -- memory
void *cross_alloc(uint32_t size)
{
	HANDLE process_heap = GetProcessHeap();
	return HeapAlloc(process_heap, HEAP_ZERO_MEMORY, size);
}
#endif

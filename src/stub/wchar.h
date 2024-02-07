#pragma once
size_t wcslen(const wchar_t *s);
size_t wcslen(const wchar_t *s) { size_t len = 0; while(*s++) len++; return len; }

#pragma once
#define _CRT_SECURE_NO_WARNINGS
#include <stdint.h>
#include <string.h>

#define ARRAY_LENGTH(x) sizeof(x) / sizeof(x[0])

inline bool string_equals(const char *string1, uint64_t length1, const char *string2, uint64_t length2)
{
	if (length1 != length2) {
		return false;
	}

	const char *end1 = string1 + length1;
	for (; string1 < end1;) {
		if (*string1 != *string2) {
			return false;
		}
		string1 += 1;
		string2 += 1;
	}

	return true;
}

inline int int_from_string(const char *s, uint64_t length)
{
	int n = 0;
	const char *end = s + length;
	while (s < end) {
		n = n * 10 + int(*s - '0');
		s += 1;
	}
	return n;
}

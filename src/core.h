#pragma once
#define _CRT_SECURE_NO_WARNINGS
#include <stdint.h>
#include <string.h>

#define ARRAY_LENGTH(x) sizeof(x) / sizeof(x[0])

struct span
{
	uint64_t start;
	uint64_t end;
};

inline span span_extend(span a, span b)
{
	return span{
		a.start < b.start ? a.start : b.start,
		a.end > b.end ? a.end : b.end,
	};
}

inline uint64_t span_length(span s)
{
	if (s.start <= s.end) {
		return s.end - s.start;
	} else {
		return 0;
	}
}

struct sv
{
	const char *chars;
	uint64_t length;
};

inline sv sv_from_null_terminated(const char *null_terminated)
{
	return sv{null_terminated, strlen(null_terminated)};
}

inline sv sv_substr(sv s, uint64_t offset, uint64_t length)
{
	return sv{s.chars + offset, length};
}

inline sv sv_substr(sv s, span span)
{
	return sv{s.chars + span.start, span_length(span)};
}

inline bool sv_equals(sv a, sv b)
{
	if (a.length != b.length) {
		return false;
	}

	const char *a_chars = a.chars;
	const char *b_chars = b.chars;
	const char *a_end = a.chars + a.length;
	for (; a_chars < a_end;) {
		if (*a_chars != *b_chars) {
			return false;
		}
		a_chars += 1;
		b_chars += 1;
	}

	return true;
}

inline int32_t sv_to_int(sv string)
{
	int32_t n = 0;
	const char *cursor = string.chars;
	const char *end = cursor + string.length;
	while (cursor < end) {
		n = n * 10 + int(*cursor - '0');
		cursor += 1;
	}
	return n;
}

inline sv sv_offset(sv string, uint64_t offset)
{
	if (offset > string.length) {
		offset = string.length;
	}
	return sv{string.chars + offset, string.length - offset};
}

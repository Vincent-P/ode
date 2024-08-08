#pragma once

#define ARRAY_LENGTH(x) sizeof(x) / sizeof(x[0])

struct span
{
	uint32_t start;
	uint32_t end;
};
typedef struct span span;

inline span span_extend(span a, span b)
{
	return (span){
		a.start < b.start ? a.start : b.start,
		a.end > b.end ? a.end : b.end,
	};
}

inline uint32_t span_length(span s)
{
	if (s.start <= s.end) {
		return s.end - s.start;
	} else {
		return 0;
	}
}

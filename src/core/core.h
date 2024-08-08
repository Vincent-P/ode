#pragma once

#if defined(_MSC_VER)
#define ASSERT(c)  if (!(c)) __debugbreak()
#else
#define ASSERT(c)  while (!(c)) __builtin_unreachable()
#endif

#include "./span.h"
#include "./string.h"
#include "./arena.h"

#pragma once

#include <cstdint>
#include <cstdarg>
#include <memory>
#include <string>
#include <vector>
#include <list>
#include <stack>

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * (UINT8_MAX + 1))

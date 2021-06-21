#include "stddef.h"

struct GenAllocResult {
    void* ptr;
    size_t generation;
};

struct GenAllocResult gen_alloc(size_t size);
void gen_free(void* ptr, size_t size);
size_t gen_generation(void* ptr);

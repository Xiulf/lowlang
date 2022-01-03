#include "stddef.h"

#ifndef include_runtime_gen_alloc
#define include_runtime_gen_alloc

typedef struct GenBoxS {
    void* ptr;
    size_t generation;
} GenBox;

GenBox gen_alloc(size_t size);
void gen_free(void* ptr, size_t size);
size_t gen_generation(void* ptr);

#endif

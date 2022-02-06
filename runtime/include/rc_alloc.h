#include "stddef.h"
#include "metadata.h"

#ifndef include_runtime_rc_alloc
#define include_runtime_rc_alloc

typedef struct RcBoxS {
    size_t strong_count;
    size_t weak_count;
    char val[];
} RcBox;

typedef struct RcBoxTypeS {
    ValueWitnessTable* vwt;
    size_t flags;
    Type* T;
} RcBoxType;

RcBox* rc_alloc(size_t size);

#endif

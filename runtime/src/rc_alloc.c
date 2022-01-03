#include "../include/rc_alloc.h"
#include "malloc.h"

RcBox* rc_alloc(size_t size) {
    RcBox* box = (RcBox*)malloc(sizeof(RcBox) + size);

    box->strong_count = 1;
    box->weak_count = 0;

    return box;
}

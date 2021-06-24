#include "../include/metadata.h"
#include "../include/gen_alloc.h"
#include "memory.h"

void copy_zst(Opaque* dst, Opaque* src, Type* t);
void move_zst(Opaque* dst, Opaque* src, Type* t);
void drop_owned_box(Opaque* val, Type* t);

ValueWitnessTable OWNED_BOX_VWT = {
    .size = sizeof(size_t) * 2,
    .align = sizeof(size_t),
    .stride = sizeof(size_t) * 2,
    .copy = copy_trivial, // this should really crash the program
    .move = move_trivial,
    .drop = drop_owned_box,
};

ValueWitnessTable UNOWNED_BOX_VWT = {
    .size = sizeof(size_t) * 2,
    .align = sizeof(size_t),
    .stride = sizeof(size_t) * 2,
    .copy = copy_trivial,
    .move = move_trivial,
    .drop = drop_trivial,
};

ValueWitnessTable TRIVIAL_VWT[6] = {
    {
        .size = 0,
        .align = 1,
        .stride = 0,
        .copy = copy_zst,
        .move = move_zst,
        .drop = drop_trivial,
    },
    {
        .size = 1,
        .align = 1,
        .stride = 1,
        .copy = copy_trivial,
        .move = move_trivial,
        .drop = drop_trivial,
    },
    {
        .size = 2,
        .align = 2,
        .stride = 2,
        .copy = copy_trivial,
        .move = move_trivial,
        .drop = drop_trivial,
    },
    {
        .size = 4,
        .align = 4,
        .stride = 4,
        .copy = copy_trivial,
        .move = move_trivial,
        .drop = drop_trivial,
    },
    {
        .size = 8,
        .align = 8,
        .stride = 8,
        .copy = copy_trivial,
        .move = move_trivial,
        .drop = drop_trivial,
    },
    {
        .size = 16,
        .align = 8,
        .stride = 16,
        .copy = copy_trivial,
        .move = move_trivial,
        .drop = drop_trivial,
    },
};

void copy_trivial(Opaque* dst, Opaque* src, Type* t) {
    memcpy(dst, src, t->vwt->size);
}

void move_trivial(Opaque* dst, Opaque* src, Type* t) {
    memcpy(dst, src, t->vwt->size);
}

void drop_trivial(Opaque* val, Type* t) {}

void copy_zst(Opaque* dst, Opaque* src, Type* t) {}
void move_zst(Opaque* dst, Opaque* src, Type* t) {}

void drop_owned_box(Opaque* val, Type* t) {
    Box* box = (Box*)val;
    gen_free(box->ptr, t->vwt->size);
}

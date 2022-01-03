#include "../include/gen_alloc.h"
#include "../include/rc_alloc.h"
#include "../include/metadata.h"
#include "memory.h"
#include "stdlib.h"

void copy_zst(Opaque* dst, Opaque* src, Type* t);
void move_zst(Opaque* dst, Opaque* src, Type* t);
void drop_owned_box(Opaque* val, Type* t);
void drop_rc_box(Opaque* val, Type* t);

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

ValueWitnessTable RC_BOX_VWT = {
    .size = sizeof(size_t),
    .align = sizeof(size_t),
    .stride = sizeof(size_t),
    .copy = copy_trivial,
    .move = move_trivial,
    .drop = drop_rc_box,
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

Type TRIVIAL_METAS[6] = {{
                             .vwt = &TRIVIAL_VWT[0],
                             .flags = TYPE_FLAG_TRIVIAL,
                         },
                         {
                             .vwt = &TRIVIAL_VWT[1],
                             .flags = TYPE_FLAG_TRIVIAL,
                         },
                         {
                             .vwt = &TRIVIAL_VWT[2],
                             .flags = TYPE_FLAG_TRIVIAL,
                         },
                         {
                             .vwt = &TRIVIAL_VWT[3],
                             .flags = TYPE_FLAG_TRIVIAL,
                         },
                         {
                             .vwt = &TRIVIAL_VWT[4],
                             .flags = TYPE_FLAG_TRIVIAL,
                         },
                         {
                             .vwt = &TRIVIAL_VWT[5],
                             .flags = TYPE_FLAG_TRIVIAL,
                         }};

void copy_trivial(Opaque *dst, Opaque *src, Type *t) {
  memcpy(dst, src, t->vwt->size);
}

void move_trivial(Opaque *dst, Opaque *src, Type *t) {
  memcpy(dst, src, t->vwt->size);
}

void drop_trivial(Opaque *val, Type *t) {}

void copy_zst(Opaque *dst, Opaque *src, Type *t) {}
void move_zst(Opaque *dst, Opaque *src, Type *t) {}

void drop_owned_box(Opaque *val, Type *t) {
  GenBox *box = (GenBox*)val;
  gen_free(box->ptr, t->vwt->size);
}

void drop_rc_box(Opaque* val, Type* t) {
    RcBox* box = (RcBox*)val;
    RcBoxType* type = (RcBoxType*)t;

    type->T->vwt->drop((Opaque*)&box->val, type->T);
    free(val);
}

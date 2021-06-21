#include "runtime.h"
#include "malloc.h"

struct FreeEntry {
    void* ptr;
    size_t generation;
    size_t size;
};

struct FreeList {
    struct FreeEntry* ptr;
    size_t len;
    size_t cap;
};

static struct FreeList FREE_LIST;

void free_list_push(struct FreeEntry entry) {
    if (!FREE_LIST.ptr) {
        FREE_LIST.ptr = (struct FreeEntry*)malloc(sizeof(struct FreeEntry));
        FREE_LIST.cap = 1;
    }

    FREE_LIST.len += 1;

    if (FREE_LIST.len > FREE_LIST.cap) {
        FREE_LIST.cap *= 2;
        FREE_LIST.ptr = (struct FreeEntry*)malloc(sizeof(struct FreeEntry) * FREE_LIST.cap);
    }

    FREE_LIST.ptr[FREE_LIST.len - 1] = entry;
}

struct FreeEntry* free_list_find(size_t size) {
    for (int i = 0; i < FREE_LIST.len; i++) {
        struct FreeEntry* entry = &FREE_LIST.ptr[i];

        if (entry->ptr && entry->size == size) {
            return entry;
        }
    }

    return NULL;
}

__attribute__((destructor)) void destroy_free_list() {
    size_t ptr_size = sizeof(size_t);

    for (int i = 0; i < FREE_LIST.len; i++) {
        struct FreeEntry entry = FREE_LIST.ptr[i];

        if (entry.ptr) {
            free(entry.ptr - ptr_size);
        }
    }

    free(FREE_LIST.ptr);
}

size_t next_power_of_2(size_t x) {
    size_t value = 1;

    while (value < x) {
        value <<= 1;
    }

    return value;
}

struct GenAllocResult gen_alloc(size_t size) {
    size_t ptr_size = sizeof(size_t);
    size_t new_size = next_power_of_2(ptr_size + size);
    struct FreeEntry* free_entry = free_list_find(new_size);
    struct GenAllocResult result;

    if (free_entry) {
        result.ptr = free_entry->ptr;
        result.generation = free_entry->generation;
        free_entry->ptr = NULL;
    } else {
        void* ptr = malloc(new_size);
        *(size_t*)ptr = 0;
        result.ptr = ptr + ptr_size;
        result.generation = 0;
    }

    return result;
}

void gen_free(void* ptr, size_t size) {
    size_t ptr_size = sizeof(size_t);
    size_t new_size = next_power_of_2(ptr_size + size);
    size_t* new_ptr = (size_t*)(ptr - ptr_size);

    *new_ptr += 1;

    for (int i = 0; i < FREE_LIST.len; i++) {
        struct FreeEntry* entry = &FREE_LIST.ptr[i];

        if (!entry->ptr) {
            entry->ptr = ptr;
            entry->generation = *new_ptr;
            return;
        }
    }

    struct FreeEntry entry;

    entry.size = new_size;
    entry.ptr = ptr;
    entry.generation = *new_ptr;

    free_list_push(entry);
}

size_t gen_generation(void* ptr) {
    size_t ptr_size = sizeof(size_t);
    size_t* new_ptr = (size_t*)(ptr - ptr_size);

    return *new_ptr;
}

#include "include/runtime.h"
#include "stdio.h"

int main() {
    ValueWitnessTable* vwt = &OwnedBoxVWT;

    printf("%lu\n", vwt->size);
    return 0;
}

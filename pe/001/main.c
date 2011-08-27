#include <stdio.h>
#include <stdint.h>

uint64_t acc;

int main (void)
{
    acc = 0;

    int i;
    for (i=1; i<1000; i++) {
        if (0 == (i%3)) {
            acc += i;
        }
    }
    for (i=1; i<1000; i++) {
        if ((0 == (i%5)) && (0 != (i%3))) {
            acc += i;
        }
    }
    printf("acc = %llu\n", acc);

    return 0;
}

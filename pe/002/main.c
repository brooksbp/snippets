#include <stdio.h>
#include <stdint.h>

int main (void)
{
    uint64_t runner_up = 1;
    uint64_t winner = 2;
    uint64_t accomplished = 0;
    uint64_t sum = 2;

    while (accomplished <= 4000000) {

        if (0 == (accomplished % 2)) {
            sum += accomplished;
        }

        accomplished = winner + runner_up;

        runner_up = winner;
        winner = accomplished;
    }
    
    printf("sum = %llu\n", sum);

    return 0;
}

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

static void
print_a (uint8_t *s, int n)
{
    for (; n > 0; n--, s++)
        printf("%02X ", *s);
    printf("\n");
}

static void
insertion_sort (uint8_t *s, int n)
{
    int i, j;
    for (i = 1; i < n; i++) {
        j = i;
        while ((j > 0) && (s[j] < s[j-1])) {
            {
                uint8_t tmp = s[j];
                s[j] = s[j-1];
                s[j-1] = tmp;
            }
            j -= 1;
        }
    }
}

int
main (int argc, char **argv)
{
    uint8_t a[] = { 5, 7, 2, 4, 8, 1, 9, 0 };

    print_a((uint8_t *) &a, sizeof(a));
    insertion_sort((uint8_t *) &a, sizeof(a));
    print_a((uint8_t *) &a, sizeof(a));

    return EXIT_SUCCESS;
}

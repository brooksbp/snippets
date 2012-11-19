/*
 * Horrible, horrible brute force algorithms to generate some
 * prime numbers and then multiply increasing permutations to
 * try to find prime factors of N. Then we try to spot
 * largest one.
 */
#include <stdio.h>
#include <stdint.h>


#define N 600851475143;
#define GEN_PRIMES_BELOW 50


int main (void)
{
    uint64_t primes[1000];
    int idx = 0;
    
    uint64_t i, j;
    int is_prime;
    for (i=1; i<GEN_PRIMES_BELOW; i++) {
        is_prime = 1;
        for (j=2; j<i; j++) {
            if ((i%j) == 0) {
                is_prime = 0;
                break;
            }
        }        
        //printf("i = %llu, prime? %s\n", i, is_prime? "YES":"NO");
        if (is_prime) {
            primes[idx++] = i;
        }
    }

    return 0;
}

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


uint64_t factorial (n)
{
    if (n == 0) {
	return 1;
    } else {
	return n * factorial(n-1);
    }
}

uint64_t fact_tail (n, acc)
{
    if (n == 0) {
	return acc;
    } else {
	return fact_tail(n-1, n*acc);
    }
}
uint64_t _factorial (n) { return fact_tail(n, 1); }



int main (int argc, char *argv[])
{
    assert(argc == 2);
    uint64_t tmp;
    uint64_t i=0;
    uint64_t max=atol(argv[1]);
    for (; i<max; i++) {
	tmp = factorial(12);
	assert(tmp == _factorial(12));
    }

    return 0;
}

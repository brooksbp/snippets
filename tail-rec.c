#include <stdio.h>



/* recursive factorial */
int factorial (n)
{
    if (n == 0) {
	return 1;
    } else {
	return n * factorial(n-1);
    }
}

/* int fact_tail (n, acc) */
/* { */
/*     if (n == 0) { */
/* 	return acc; */
/*     } else { */
/* 	return fact_tail(n-1, n*acc); */
/*     } */
/* } */
int fact_tail (n, acc)
{
  begin:
    if (n == 0) {
	return acc;
    } else {
	...
}
int _factorial (n) { fact_tail(n, 1); }



int main (void)
{
    return 0;
}

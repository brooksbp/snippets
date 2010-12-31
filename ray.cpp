// http://fuzzyphoton.tripod.com/howtowrt.htm

#include <stdio.h>

#define LARGE_VAL 1e10


int sign(double x)
{
    return (x == 0 ? 0 : (x < 0 ? -1 : 1));
}


// returns a/b (0/0 = 0, overflow = LARGE_VAL with correct sign)
double fdiv(double a, double b)
{
    if (b == 0) {
	if (a == 0)
	    return 0;
	else
	    return LARGE_VAL * sign(a);
    } else {
	if (a == 0)
	    return 0;
	else {
	    if ((a + b) == a)
		return LARGE_VAL * sign(a) * sign(b);
	    else
		return a/b;
	}
    }
}


int main()
{
    printf("hello world!\n");
    return 0;
}

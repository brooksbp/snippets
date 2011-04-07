/*
 * function pointers - pointers that point to the address of a function.
 *
 * functions and function pointers with different calling conventions
 * are incompatible with each other.
 */

#include <stdio.h>


int func (int a, char *str)
{
    printf("%s\n", str);
    return a;
}

void call_fp (int (*f)(int a, char *str))
{
    int r3 = f(13, "foobar");
}

/* return_fp takes an int parameter and returns a pointer to a function that
 * takes an int and char and returns an int */
int (*return_fp(int b))(int, char *)
{
    return &func;
}

int main (void)
{
    /* define fp and initialize to NULL */
    int (*fp)(int, char *) = NULL;

    /* assign fp */
    fp = &func;
    
    /* call fp */
    int r1 = fp(12, "hello world");
    int r2 = (*fp)(12, "hello world");

    /* pass fp */
    call_fp(fp);

    /* assign returned fp */
    fp = return_fp(15);

    return 0;
}


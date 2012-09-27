#include <alloca.h>
#include <stdio.h>

static void fun(char *p2, int incr)
{
  int z = 4;
  printf("p2+incr+z=0x%p", p2 + incr + z);
}

int main(void)
{
  char *p = alloca(8);

  printf("p=0x%p\n", p);

  char *z;
  
  fun((z = alloca(4)), 4);
  
  return 0;
}

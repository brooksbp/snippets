#include <stdlib.h>
#include <stdio.h>

int main(void)
{
  char *p = malloc(8);
  int i;
  
  for (i = 0; i < 128; i++)
    printf("0x%p=0x%x\n", p-i, *(p-i));

  free(p);

  for (i = 0; i < 128; i++)
    printf("0x%p=0x%x\n", p-i, *(p-i));
  
  return 0;
}

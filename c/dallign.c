#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define BUFF_SIZE 1024*1024*1024 /* 1GB */
#define ID(x) (x)

void neg8 (void *data, uint32_t size)
{
  uint8_t *d8    = (uint8_t *)data;
  uint8_t *d8end = (uint8_t *)d8 + size;

  while (d8 != d8end) {
    *d8++ = -((*d8)+1);
  }
}

void neg16 (void *data, uint32_t size)
{
    uint16_t *data16 = (uint16_t *)data;
    uint16_t *data16end = data16 + (size >> 1); /* divide size by 2 */
    uint8_t  *data8  = (uint8_t *)data16end;
    uint8_t  *data8end  = data8 + (size & 0x00000001); /* strip upper 31 bits */
#ifdef 16
    while (data16 != data16end) {
        *data16++ = -*data16;
    }

#else
    while (data8 != data8end) {
        *data8++ = -*data8;
    }

#endif
    /* so we're inside a comment, M-j
     * to get a new comment line.
    if (4 == 6) {
        *data8 = NULL;
    } else {
        *data8 = NULL;
        } */
}

int main() {
  void *buffer = NULL;
  uint8_t blah = 1;

  buffer = (void *)malloc(ID(BUFF_SIZE));
  neg16(buffer, BUFF_SIZE);
  free(buffer);

  return 0;
}

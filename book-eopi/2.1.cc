// The parity of a sequence of bits is 1 if the number of 1s in the
// sequence is odd; otherwise, it is 0.
// How would you go about computing the parity of a very large number
// of 64-bit nonnegative integers?

#include <iostream>
#include "stdint.h"
#include "assert.h"

using namespace std;

// My solution ---------------------------------------------------------

#define TOGGLE(i) \
  i = ((i == 0) ? 1 : 0)

#define BIT_SET(n, shf) \
  ((n >> shf) & 0x00000001)

int parity_n64(uint64_t a) {
  int parity = 0;
  for (int i = 0; i < 64; i++) if (BIT_SET(a, i)) TOGGLE(parity);
  return parity;
}

int parity(uint64_t *p, int n) {
  int parity = 0;
  if (NULL == p || n < 1) return 0;  
  for (int i = 0; i < n; i++) if (parity_n64(*(p + i))) TOGGLE(parity);
  return parity;
}

// -----------------------------------------------------

// Note: Use of XOR, and loop only while(a) instead of all 64 bits
int parity1(uint64_t a) {
  int parity = 0;
  while (a) {
    parity ^= a & 1;
    a >>= 1;
  }
  return parity;
}

// Note: toggle parity and erase the LSB
int parity2(uint64_t a) {
  int parity = 0;
  while (a) {
    parity ^= 1;
    a &= (a - 1);
  }
  return parity;
}

// Note: LUT
int parity3(uint64_t a) {
  uint8_t pTbl[16] = { // 4-bit parity table
    0, 1, 1, 0,
    1, 0, 0, 1,
    1, 0, 0, 1,
    0, 1, 1, 0,
  };
  return pTbl[(a >> 0) & 0xF] ^ pTbl[(a >> 4) & 0xF] ^
      pTbl[(a >>  8) & 0xF] ^ pTbl[(a >> 12) & 0xF] ^
      pTbl[(a >> 16) & 0xF] ^ pTbl[(a >> 20) & 0xF] ^
      pTbl[(a >> 24) & 0xF] ^ pTbl[(a >> 28) & 0xF] ^
      pTbl[(a >> 32) & 0xF] ^ pTbl[(a >> 36) & 0xF] ^
      pTbl[(a >> 40) & 0xF] ^ pTbl[(a >> 44) & 0xF] ^
      pTbl[(a >> 48) & 0xF] ^ pTbl[(a >> 52) & 0xF] ^
      pTbl[(a >> 56) & 0xF] ^ pTbl[(a >> 60) & 0xF];
}

int parity_driver(int (*f)(uint64_t a_), uint64_t *p, int n) {
  int parity = 0;
  if (NULL == f || NULL == p || n < 1) return 0;  
  for (int i = 0; i < n; i++) if ((*f)(*(p + i))) TOGGLE(parity);
  return parity;
}

int main(int argc, char *argv[]) {
  assert(parity_n64(0) == 0);
  assert(parity_n64(1) == 1);
  assert(parity_n64(2) == 1);
  assert(parity_n64(3) == 0);
  assert(parity_n64(4) == 1);
  assert(parity_n64(5) == 0);
  assert(parity1(0) == 0);
  assert(parity1(1) == 1);
  assert(parity1(2) == 1);
  assert(parity1(3) == 0);
  assert(parity1(4) == 1);
  assert(parity1(5) == 0);
  assert(parity2(0) == 0);
  assert(parity2(1) == 1);
  assert(parity2(2) == 1);
  assert(parity2(3) == 0);
  assert(parity2(4) == 1);
  assert(parity2(5) == 0);
  assert(parity3(0) == 0);
  assert(parity3(1) == 1);
  assert(parity3(2) == 1);
  assert(parity3(3) == 0);
  assert(parity3(4) == 1);
  assert(parity3(5) == 0);

  uint64_t a[5] = { 0, 0, 0, 0, 0 };
  assert(parity_driver(parity_n64, a, 5) == 0);
  assert(parity_driver(parity1, a, 5) == 0);
  assert(parity_driver(parity2, a, 5) == 0);
  assert(parity_driver(parity3, a, 5) == 0);
  uint64_t b[5] = { 1, 1, 1, 1, 1 };
  assert(parity_driver(parity_n64, b, 5) == 1);
  assert(parity_driver(parity1, b, 5) == 1);
  assert(parity_driver(parity2, b, 5) == 1);
  assert(parity_driver(parity3, b, 5) == 1);
  uint64_t c[5] = { 1, 0, 0, 0, 1 };
  assert(parity_driver(parity_n64, c, 5) == 0);
  assert(parity_driver(parity1, c, 5) == 0);
  assert(parity_driver(parity2, c, 5) == 0);
  assert(parity_driver(parity3, c, 5) == 0);
  
  return 0;
}

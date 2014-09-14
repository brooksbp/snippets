// Define the number of bits that are set to 1 in an unsigned 64-bit
// integer x to be the weight of x. Let S_k denote the set of unsigned
// 64-bit integers whose weight is k.
// Suppose x \in S_k, and k is not 0 or 64. How would you compute
// y \in S_k \ {x} such that |y - x| is minimum?

#include <iostream>
#include "stdint.h"
#include "assert.h"

int weight(uint64_t x) {
  int weight_tbl[16] = {
    0, 1, 1, 2,
    1, 2, 2, 3,
    1, 2, 2, 3,
    2, 3, 3, 4,
  };
  return weight_tbl[(x >> 0) & 0xF] +
      weight_tbl[(x >> 4) & 0xF] +
      weight_tbl[(x >> 8) & 0xF] +
      weight_tbl[(x >> 12) & 0xF] +
      weight_tbl[(x >> 16) & 0xF] +
      weight_tbl[(x >> 20) & 0xF] +
      weight_tbl[(x >> 24) & 0xF] +
      weight_tbl[(x >> 28) & 0xF] +
      weight_tbl[(x >> 32) & 0xF] +
      weight_tbl[(x >> 36) & 0xF] +
      weight_tbl[(x >> 40) & 0xF] +
      weight_tbl[(x >> 44) & 0xF] +
      weight_tbl[(x >> 48) & 0xF] +
      weight_tbl[(x >> 52) & 0xF] +
      weight_tbl[(x >> 56) & 0xF] +
      weight_tbl[(x >> 60) & 0xF];
}

int main(int argc, char *argv[]) {
  assert(weight(0xF0000000) == 4);

  // Pick y to be x where LSB left-shifted. Or,
  // from LSB to MSB, the first '01' sequence, and
  // swap bits.
  
  return 0;
}

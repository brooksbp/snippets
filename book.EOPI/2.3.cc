// Write a function that takes a 64-bit integer x and returns a
// 64-bit integer consisting of the bits of x in reverse order.

#include <iostream>
#include "stdint.h"
#include "assert.h"

uint64_t reverse(uint64_t x) {
  // Precomputed reverse table
  return x;
}

int main(int argc, char *argv[]) {
  assert(reverse(1ULL) == 0x8000000000000000);
  return 0;
}

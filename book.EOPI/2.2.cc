// function that takes a 64-bit integer x and swaps the bits at
// indices i and j where LSB is index 0.

#include <iostream>
#include "stdint.h"
#include "assert.h"

using namespace std;

uint64_t swap(uint64_t x, int i, int j) {
  if (i > 63 || i < 0 || j > 63 || i < 0) return x;
  return (x & ~((1ULL << i) | (1ULL << j))) |
      (((x >> i) & 1) << j) |
      (((x >> j) & 1) << i);
}

uint64_t swap2(uint64_t x, int i, int j) {
  if (((x >> i) & 1ULL) != ((x >> j) & 1ULL)) {
    return x ^ ((1ULL << i) | (1ULL << j));
  }
  return x;
}

int main(int argc, char *argv[]) {
  assert(swap(0x0000000000000100, 0, 8) == 0x0000000000000001);
  assert(swap(0xF000000000000000, 63, 0) == 0x7000000000000001);
  assert(swap2(0x0000000000000100, 0, 8) == 0x0000000000000001);
  assert(swap2(0xF000000000000000, 63, 0) == 0x7000000000000001);
  return 0;
}


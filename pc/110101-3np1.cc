#include <algorithm>
#include <iostream>
#include <stdint.h>

using namespace std;

uint64_t cycle_length(uint64_t n) {
  if (n <= 1) return 1;
  return 1 + cycle_length((n & 0x1) ? n * 3 + 1 : n / 2);
}

uint64_t solve(int i, int j) {
  if (i > j) swap(i, j);
  uint64_t max = 0;
  for (int k = i; k <= j; k++) {
    uint64_t len = cycle_length((uint64_t) k);
    if (len > max) max = len;
  }
  return max;
}

int main(int argc, char *argv[]) {
  int i, j;
  while (cin >> i >> j) {
    cout << i << " " << j << " " << solve(i, j) << endl;
  }
  return 0;
}

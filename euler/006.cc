#include "inc.h"

static uint64_t sumofsquares(int n) {
  uint64_t acc = 0;
  do { acc += n * n; } while (n--);
  return acc;
}

static uint64_t squareofsum(int n) {
  uint64_t acc = 0;
  do { acc += n; } while (n--);
  return acc * acc;
}

int main(int argc, char *argv[]) {
  assert(sumofsquares(10) == 385);
  assert(squareofsum(10) == 3025);

  cout << (squareofsum(100) - sumofsquares(100)) << endl;
  
  return 0;
}

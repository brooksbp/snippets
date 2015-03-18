#include <stdlib.h>
#include <math.h>
#include <assert.h>

// Is x a power of y?
//
// log_y(x) should be an integer.
//
int is_power(int x, int y) {
  // log_a(x) == log_c(x) / log_c(a)
  //
  // log_2(100) == log_10(100) / log_10(2)

  double d = log(abs(x)) / log(abs(y));

  if ((x > 0 && y > 0) || (x < 0 && y < 0)) {
    if (d == (int) d) {
      return 1;
    } else {
      return -1;
    }
  } else if (x > 0 && y < 0) {
    if ((int) d % 2 == 0) {
      return 1;
    } else {
      return -1;
    }
  }
  return -1;
}

int main() {
  assert(1 == is_power(8, 2));
  assert(1 == is_power(4096, 2));
  assert(1 == is_power(9, 3));
  assert(-1 == is_power(10, 3));
  //assert(1 == is_power(10, 3));
  return 0;
}

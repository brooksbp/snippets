#include "inc.h"

#define N 1000

int main(void) {
  int acc = 0;
  for (int i = 0; i < N; i++) {
    if ((i % 3) == 0 || (i % 5) == 0) {
      acc += i;
    }
  }
  cout << acc;
  return 0;
}

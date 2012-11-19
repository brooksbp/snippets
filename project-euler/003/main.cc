#include <iostream>
#include <cmath>

using namespace std;

void prime_factorization(long long x) {
  long long i, c;

  c = x;

  while ((c % 2) == 0) {
    c = c / 2;
  }

  i = 3;
  while (i <= (sqrt(c) + 1)) {
    if ((c % i) == 0) {
      cout << i << endl;
      c = c / i;
    } else {
      i = i + 2;
    }
  }

  if (c > 1) {
    cout << c << endl;
  }
}

int main(void) {
  prime_factorization(600851475143);
  return 0;
}

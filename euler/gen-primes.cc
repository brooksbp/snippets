#include "inc.h"

void gp1(void) {
  for (int i = 2; i < 100; i++) {
    for (int j = 2; j*j <= i; j++) {
      if (i % j == 0)
        break;
      else if (j + 1 > sqrt(i)) {
        cout << i << " ";
      }
    }
  }
}

void gp2(void) {
  for (int i = 2; i < 100; i++) {
    bool prime = true;
    for (int j = 2; j * j <= i; j++) {
      if (i % j == 0) {
        prime = false;
        break;
      }
    }
    if (prime)
      cout << i << " ";
  }
}

void gp3(void) {
  vector<int> primes;

  primes.push_back(2);
  cout << "2 ";

  // if a number is divisible by a non-prime number, there is also
  // some prime <= that divisor which it is also divisible by.
  // Reduces computation by a factor of primes_in_range/total_range.
  
  for (int i = 3; i < 100; i++) {
    bool prime = true;
    for (int j = 0; j < primes.size() && primes[j] * primes[j] <= i; j++) {
      if (i % primes[j] == 0) {
        prime = false;
        break;
      }
    }
    if (prime) {
      primes.push_back(i);
      cout << i << " ";
    }
  }
}

int main(int argc, char *argv[]) {
  gp1(); cout << endl;
  gp2(); cout << endl;
  gp3(); cout << endl;
  return 0;
}

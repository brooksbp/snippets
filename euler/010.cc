#include "inc.h"

int main(int argc, char *argv[]) {
  vector<int> primes;
  primes.push_back(2);
  uint64_t acc = 2;

  for (int i = 3; i < 2000000; i++) {
    bool prime = true;
    for (int j = 0; j < primes.size() && primes[j] * primes[j] <= i; j++) {
      if (i % primes[j] == 0) {
        prime = false;
        break;
      }
    }
    if (prime) {
      primes.push_back(i);
      acc += i;
    }
  }
  cout << acc;
  
  return 0;
}

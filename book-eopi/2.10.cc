// Design an algorithm for computing the GCD of two numbers without
// using multiplication, division, or the modulus operators.

#include <iostream>
#include <unordered_map>
#include <cassert>

using namespace std;

int gcd(int a, int b) {
  if (a == b) {
    return a;
  }
  
  unordered_map<int, int> divisors;

  // Divisibly by 1 and itself.
  divisors[1] += 1;
  divisors[a] += 1;

  int d = a / 2 + 1;
  while (d > 1) {
    int x = a;
    while (x > 1) x -= d;
    if (x == 0) divisors[d] += 1;
    d -= 1;
  }

  d = b / 2 + 1;
  while (d > 1) {
    int x = b;
    while (x > 1) x -= d;
    if (x == 0) {
      divisors[d] += 1;
      if (divisors[d] > 1) {
        return d;
      }
    }
    d -= 1;
  }
  
  return 1;
}

// EOPI optimizations:
// - if even, divide (shiftR) by 2
// - if odd, subtract smaller from larger "and return
//   gcd of resulting pair"

int main(int argc, char *argv[]) {
  assert(gcd(10, 80) == 10);
  assert(gcd(13, 5) == 1);
  return 0;
}

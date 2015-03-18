#include <iostream>
#include <vector>
#include <set>
#include <utility>
#include <stdint.h>
#include <algorithm>
#include <numeric>

using namespace std;
#if 1
uint64_t gcd(uint64_t a, uint64_t b) {
  if (a == 0) {
    return b;
  } else if (b == 0) {
    return a;
  } else if (!(a & 1) && !(b & 1)) {
    return gcd(a >> 1, b >> 1) << 1;
  } else if (!(a & 1) && b & 1) {
    return gcd(a >> 1, b);
  } else if (a & 1 && !(b & 1)) {
    return gcd(a, b >> 1);
  } else if (a > b) {
    return gcd(a - b, b);
  }
  return gcd(a, b - a);
}
#endif
#if 0
uint64_t gcd(uint64_t a, uint64_t b) {
  for (;;) {
    if (a == 0) return b;
    b %= a;
    if (b == 0) return a;
    a %= b;
  }
}
#endif
uint64_t lcm(uint64_t a, uint64_t b) {
#if 0
  uint64_t t = gcd(a, b);
  return t ? (a / t * b) : 0;
#endif
  return (a * b) / gcd(a, b);
}

int main() {
  vector<uint64_t> V;

  for (uint64_t i = 1; i <= 99; i++) {
    if (i % 2 == 0) continue;

    V.push_back(i);
  }

  for (auto p : V) cout << p << " ";
  cout << endl;
  cout << accumulate(V.begin(), V.end(), 1, lcm);

#if 0
  for (auto e : V) cout << e << " ";
  cout << endl;

  uint64_t a = 99;

  for (;;) {
    bool found = true;
    for (auto p : V) {
      if (a % p) {
        found = false;
        break;
      }
    }
    if (found) break;
    a += V.front();
    //cout << a << endl;
  }
  cout << a << endl;
#endif

  
  return 0;
}

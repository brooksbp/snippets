#include <iostream>

using namespace std;

#define N 1000

int main(void) {
  int acc = 0;

  int m3, m5;

  int i = 1;
  while (++i) {
    m3 = i * 3;
    if (m3 < N) {
      acc += m3;
    } else {
      break;
    }
  }

  i = 1;
  while (i++) {
    m5 = i * 5;
    if (m5 < N) {
      if ((m5 % 3) != 0) {
        acc += m5;
      }
    } else {
      break;
    }
  }
  
  cout << acc;
  return 0;
}

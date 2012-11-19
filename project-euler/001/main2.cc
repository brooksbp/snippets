#include <iostream>

using namespace std;

#define N 1000

int main(void) {
  int acc = 0;

  int m3, m5;
  for (int i = 0; i < N; i++) {
    m3 = 3 * i;
    m5 = 5 * i;
    if (m3 < N) {
      acc += m3;
    }
    if (m5 < N && (m5 % 3) != 0) {
      acc += m5;
    }
  }
  
  cout << acc;
  return 0;
}

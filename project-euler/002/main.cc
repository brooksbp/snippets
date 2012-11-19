#include <iostream>

using namespace std;

int main(void) {

  int prevprev = 1, prev = 2, cur;
  int acc = 2; // prev is even

  do {
    cur = prevprev + prev;
    prevprev = prev;
    prev = cur;
    if ((cur % 2) == 0) {
      acc += cur;
    }
  } while (cur < 4000000);
  // if 4000000 was prime, acc -= cur !
  
  cout << acc;
  
  return 0;
}

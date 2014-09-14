#include "inc.h"

int main(void) {
  long long n = 21;
#define edn(a) ((n % a) == 0)
  while (n++) {
    if(edn(1) && edn(2) && edn(3) && edn(4) && edn(5) && edn(6) && edn(7) &&
       edn(8) && edn(9) && edn(10) && edn(11) && edn(12) && edn(13) && edn(14) &&
       edn(15) && edn(16) && edn(17) && edn(18) && edn(19) && edn(20)) {
      cout << n << endl;
      break;
    }
  }
  return 0;
}

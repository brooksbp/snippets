#include "inc.h"

int main(int argc, char *argv[]) {
  for (int a = 1; a < 1000; a++) {
    for (int b = a + 1; b < 1000; b++) {
      for (int c = b + 1; c < 1000; c++) {
        if (((a * a) + (b * b)) == (c * c)) {
          if ((a + b + c) == 1000) {
            cout << (a * b * c);
          }
        }
      }
    }
  }
  return 0;
}

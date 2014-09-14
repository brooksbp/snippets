#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {
  int i;
  for (;;) {
    cin >> i;
    if (i == 42) return 0;
    cout << i << endl;
  }
  return 0;
}

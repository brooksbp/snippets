#include <iostream>
#include <string>
using namespace std;

int main(void) {
  string str("my string");
  string::reverse_iterator rit;
  for (rit = str.rbegin(); rit < str.rend(); rit++) {
    cout << *rit;
  }
  return 0;
}

// Write a function that converts Excel column ids to the corresponding
// integer, with "A" corresponding to 1.

#include <iostream>
#include <string>

using namespace std;

int ssDecodeColID(const string &s) {
  int x = 0;
  int power = 1;
  for (auto rit = s.crbegin(); rit != s.crend(); rit++) {
    x += power * (*rit - 'A' + 1);
    power *= 26;
  }
  return x;
}

int ssDecodeColID2(const string &col) {
  int ret = 0;
  for (const char &c : col) {
    ret = ret * 26 + c - 'A' + 1;
  }
  return ret;
}

int main(int argc, char *argv[]) {
  cout << ssDecodeColID("A") << endl;
  cout << ssDecodeColID("Z") << endl;
  cout << ssDecodeColID("AA") << endl;
  cout << ssDecodeColID("AZ") << endl;
  cout << ssDecodeColID("BA") << endl;
  cout << ssDecodeColID("BZ") << endl;
  cout << ssDecodeColID("ZZ") << endl;
  cout << ssDecodeColID("AAA") << endl;
  cout << ssDecodeColID2("A") << endl;
  cout << ssDecodeColID2("Z") << endl;
  cout << ssDecodeColID2("AA") << endl;
  cout << ssDecodeColID2("AZ") << endl;
  cout << ssDecodeColID2("BA") << endl;
  cout << ssDecodeColID2("BZ") << endl;
  cout << ssDecodeColID2("ZZ") << endl;
  cout << ssDecodeColID2("AAA") << endl;
  return 0;
}

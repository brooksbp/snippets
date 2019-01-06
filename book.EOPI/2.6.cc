// Implement string/integer inter-conversion functions.

#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

string intToString(int x) {
  string s;
  if (x == 0) {
    s.push_back('0');
    return s;
  }  
  bool negative = (x < 0);
  if (negative) {
    x = -x;
  }
  while (x) {
    s.push_back('0' + (x % 10));
    x /= 10;
  }
  if (negative) {
    s.push_back('-');
  }
  reverse(s.begin(), s.end());
  return s;
}

int stringToInt(string s) {
  int idx;
  bool negative;
  if (s[0] == '-') {
    negative = true; idx = 1;
  } else {
    negative = false; idx = 0;
  }
  int x = 0;
  while (idx < s.size()) {
    if (isdigit(s[idx])) {
      x = x * 10 + (s[idx++] - '0');
    }
  }
  return negative ? -x : x;
}

int main(int argc, char *argv[]) {
  cout << intToString(-543);
  cout << stringToInt(string("-543"));
  return 0;
}

#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

void rle_encode_append(string& s, int count, char c) {
  string n;
  while (count) {
    n += '0' + (count % 10);
    count /= 10;
  }
  reverse(n.begin(), n.end());
  s += n;
  s += c;
}

string rle_encode(const string& s) {
  string tmp;
  char c = s[0];
  int count = 1;
  if (s.length() < 1) return s;
  for (auto it = s.cbegin() + 1; it != s.cend(); it++) {
    if (*it == c) {
      count++;
    } else {
      rle_encode_append(tmp, count, c);
      c = *it;
      count = 1;
    }
  }
  rle_encode_append(tmp, count, c);
  return tmp;
}

// TODO eopi solution used stringstream.. kind of a cheat, but
// much cleaner code!

string rle_decode(const string& s) {
  string tmp;
  int n = 0;
  for (auto it = s.cbegin(); it != s.cend(); it++) {
    if (isdigit(*it)) {
      n = (n * 10) + (*it - '0');
    } else {
      tmp.append(n, *it);
      n = 0;
    }
  }
  return tmp;
}

int main(int argc, char *argv[]) {
  cout << rle_encode("") << endl;
  cout << rle_encode("a") << endl;
  cout << rle_encode("aaaaaaaaaabcccc") << endl;
  cout << rle_decode("21a1b4c") << endl;
  cout << rle_decode("1a") << endl;
  cout << rle_decode("") << endl;
  return 0;
}

// Write a function that performs base conversion. Specifically, the input
// is an integer base b_1, a string s, representing an integer x in base
// b_1, and another integer base b_2; the output is the string representing
// the integer x in base b_2. Assume 2 <= b_1, b_2 <= 16. Use "A" to represent
// 10, "B" for 11, etc.

#include <iostream>
#include <string>

using namespace std;

int char2int(char c) {
  switch (c) {
    case 'A': return 10;
    case 'B': return 11;
    case 'C': return 12;
    case 'D': return 13;
    case 'E': return 14;
    case 'F': return 15;
    default:
      return c - '0';
  }
}

char int2char(int n) {
  switch (n) {
    case 10: return 'A';
    case 11: return 'B';
    case 12: return 'C';
    case 13: return 'D';
    case 14: return 'E';
    case 15: return 'F';
    default:
      return '0' + n;
  }
}

string base_conversion(int b1, string s, int b2) {
  if (b1 == b2) {
    return s;
  }

  // Convert s in b1 to integer.
  int x = 0;
  int power = 1;
  for (string::reverse_iterator rit = s.rbegin(); rit != s.rend(); rit++) {
    x += power * char2int(*rit);
    power *= b1;
  }
  
  // Find the starting power for b2 conversion.
  power = 1;
  while (power * (b2-1) < x) {
    power *= b2;
  }
  if (power > x) { // step back
    power /= b2;
  }
  
  // R->L convert to s in b2.
  string s2("");
  while (power > 0) {
    if (power > x) {
      s2 += '0';      
    } else {
      int n = (b2 - 1);
      while ((n * power) > x) {
        n--;
      }
      s2 += int2char(n);
      x -= (n * power);
    }
    power /= b2;
  }
  
  return s2;
}

// My solution does not handle negatives, zero case,
// and oh my god is it more verbose than EOPI
// solution.. embarrassing

#include <algorithm>

string base_conversion2(const int &b1, const string &s, const int &b2) {
  bool neg = (s[0] == '-');
  int x = 0;
  for (int i = (neg == true ? 1 : 0); i < s.size(); i++) {
    x *= b1;
    x += isdigit(s[i]) ? s[i] - '0' : s[i] - 'A' + 10;
  }

  string ans;
  while (x) {
    int r = x % b2;
    ans.push_back(r >= 10 ? 'A' + r - 10 : '0' + r);
    x /= b2;
  }

  if (ans.size() == 0) {
    ans.push_back('0');
  }
  if (neg) {
    ans.push_back('-');
  }
  reverse(ans.begin(), ans.end());
  return ans;
}

int main(int argc, char *argv[]) {
  cout << base_conversion(10, "10", 2) << endl;
  cout << base_conversion(10, "223", 2) << endl;
  cout << base_conversion(10, "223", 10) << endl;
  cout << base_conversion(10, "223", 16) << endl;
  cout << base_conversion2(10, "10", 2) << endl;
  cout << base_conversion2(10, "223", 2) << endl;
  cout << base_conversion2(10, "223", 10) << endl;
  cout << base_conversion2(10, "223", 16) << endl;
  return 0;
}

// Implement an algorithm to determine if a string has all unique
// characters.  What if you cannot use additional data structures?

#include <iostream>
#include <string>
#include <assert.h>
#include <string.h>
#include <unordered_map>

using namespace std;

#define MAX_CHARS 256   // 8-bit ASCII

bool all_unique(string& str) {
  unordered_map<char,int> char_map;
  string::iterator i;
  if (str.length() > MAX_CHARS) return false;
  for (i = str.begin(); i < str.end(); i++) {
    char_map[*i] += 1;
    if (char_map[*i] > 1) return false;
  }
  return true;
}

bool all_unique2(string& str) {
  string::iterator i, j;
  if (str.length() > MAX_CHARS) return false;
  for (i = str.begin(); i < (str.end() - 1); i++)
    for (j = i + 1; j < str.end(); j++)
      if (*i == *j) return false;
  return true;
}

bool all_unique3(string& str_) {
  int char_map[MAX_CHARS] = { 0 };
  const char *str = str_.c_str();
  if (strlen(str) > MAX_CHARS) return false;
  for (int i = 0; i < strlen(str); i++) {
    char c = *(str + i);
    char_map[c] += 1;
    if (char_map[c] > 1) return false;
  }
  return true;
}

int main(int argc, char *argv[]) {
  string s1("a uniq-str");
  string s2("another");
  string empty("");
  string sc = s1 + s2 + empty;

#define RUN_ASSERTS(fn) do { \
    assert(fn(s1));          \
    assert(fn(s2));          \
    assert(fn(empty));       \
    assert(!fn(sc));         \
  } while (0)

  RUN_ASSERTS(all_unique);
  RUN_ASSERTS(all_unique2);
  RUN_ASSERTS(all_unique3);
  return 0;
}

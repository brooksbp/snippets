// Implement an algorithm to determine if a string has all unique
// characters.  What if you cannot use additional data structures?

#include <iostream>
#include <string>
#include <assert.h>
#include <string.h>
#include <unordered_map>

using namespace std;

#define MAX_CHARS 256   // 8-bit ASCII

// Hash every character detecting duplicates along the way..
// O(n), O(1) space
bool s1(string& str) {
  if (str.length() > MAX_CHARS) {
    return false;
  }
  unordered_map<char,int> char_map;
  for (string::iterator i = str.begin(); i < str.end(); i++) {
    char_map[*i] += 1;
    if (char_map[*i] > 1) {
      return false;
    }
  }
  return true;
}

// For each character, see if it exists it the remaining part of string..
// O(n^2), O(1) space
bool s2(string& str) {
  if (str.length() > MAX_CHARS) {
    return false;
  }
  for (string::iterator i = str.begin(); i < (str.end() - 1); i++)
    for (string::iterator j = i + 1; j < str.end(); j++)
      if (*i == *j) return false;
  return true;
}

// C version of s1 with an array used as hash tbl
// O(n), O(1) space
bool s3(string& str_) {
  const char *str = str_.c_str();  
  if (strlen(str) > MAX_CHARS) {
    return false;
  }
  int char_map[MAX_CHARS] = { 0 };
  for (int i = 0; i < strlen(str); i++) {
    char c = *(str + i);
    char_map[c] += 1;
    if (char_map[c] > 1) {
      return false;
    }
  }
  return true;
}

// Could also sort the string O(nlogn) and search for duplicates O(n),
// O(nlogn) + O(n) => O(n)..

// Could also use a bit vector instead of array to save space.

int main(int argc, char *argv[]) {
  string str1("a uniq-str");
  string str2("another");
  string str3("");
  string str4 = str1 + str2 + str3;

#define GO(fn) do {            \
    assert(fn(str1));          \
    assert(fn(str2));          \
    assert(fn(str3));          \
    assert(!fn(str4));         \
  } while (0)

  GO(s1);
  GO(s2);
  GO(s3);
  return 0;
}

// Implement a function void reverse(char* str) which reverses
// a null-terminated string.

#include <iostream>
#include <cstring>
#include <cassert>

using namespace std;

void reverse(char* str) {
  char *a = str;
  char *b = a + strlen(str) - 1;
  int done;
  if (strlen(str) < 2) return;
  do {
    char tmp = *a; *a = *b; *b = tmp;
    done = a == b || a == (b - 1);
    a += 1; b -= 1;
  } while (!done);
}

int main(int argc, char *argv[]) {
  char* ro_str = "123abc"; // RODATA string literal!!

#define RUN_ASSERT(strf, strr) do {    \
    string s(strf);                    \
    reverse((char *) s.c_str());       \
    assert(!s.compare(strr));          \
  } while (0)
  
  RUN_ASSERT("", "");
  RUN_ASSERT("a", "a");
  RUN_ASSERT("ab", "ba");
  RUN_ASSERT("123abc", "cba321");
  RUN_ASSERT("123xabc", "cbax321");  
  return 0;
}

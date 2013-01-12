// Write a method to replace all spaces in a string with '%20'.
// You may assume that the string has sufficient space at the
// end to hold the additional characters.

#include <iostream>
#include <string>

using namespace std;

void to_MS_spaces(char *str) {
  
}

int main(int argc, char *argv[]) {
  int i;
  
#define RUN_ASSERT(str_a, str_b) do { \
    string s(str_a);                  \
    to_MS_spaces((char *) s.c_str()); \
    assert(!s.compare(str_b));        \
  } while (0)
      
  RUN_ASSERT("", "");
  RUN_ASSERT(" ", " "); // too short
  RUN_ASSERT(" a  ", "%20a");
  RUN_ASSERT("a b c    ", "a%20b%20c");
  RUN_ASSERT("a bxx c      ", "a%20bxx%20c  ");
  
  return 0;
}

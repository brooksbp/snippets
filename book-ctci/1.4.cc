// Write a method to replace all spaces in a string with '%20'.
// You may assume that the string has sufficient space at the
// end to hold the additional characters.

#include <iostream>
#include <string>
#include <assert.h>

using namespace std;

bool all_spaces(string::iterator i, string::iterator end) {
  for (; i != end; i++) if (*i != ' ') return false;
  return true;
}

void to_MS_spaces(string& s) {
  string::iterator i = s.begin();
  string::iterator j;
  while (!all_spaces(i, s.end())) {
    if (*i != ' ') i += 1;
    else {
      for (j = s.end(); (j-3) != i; j--) {
        cout << "*j=" << *j << " *(j-2)=" << *(j-2) << endl;
        *j = *(j-2);
      }
      
      *i = '%'; *(i+1) = '2'; *(i+2) = '0';
      i += 3;
    }
  }
}


int main(int argc, char *argv[]) {
  int i;
  
#define RUN_ASSERT(str_a, str_b) do { \
    string s(str_a);                  \
    to_MS_spaces(s);                  \
    cout << "|" << s << "|" << endl;  \
    assert(!s.compare(str_b));        \
  } while (0)

  //RUN_ASSERT("", "");
  //RUN_ASSERT(" ", " "); // too short
  RUN_ASSERT(" a  ", "%20a");
  //RUN_ASSERT("a b c    ", "a%20b%20c");
  //RUN_ASSERT("a bxx c      ", "a%20bxx%20c  ");
  
  return 0;
}

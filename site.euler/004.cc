#include "inc.h"

// generate & print all palis
// ./a.out | sort -n | tail

int main(void) {

  ostringstream ostr;
  string str;
  string::iterator it;
  string::reverse_iterator rit;
  
  for (int i = 999; i > 99; i--) {
    for (int j = 999; j > 99; j--) {
      str = static_cast<ostringstream*>( &(ostringstream() << (i * j)) )->str();
      if ((str.length() % 2) != 0)
        continue;
      bool is_palindrome = true;
      for (it = str.begin(), rit = str.rbegin(); it < str.end(); it++, rit++) {
        // double the work == double the pleasure..?
        if (*it != *rit) {
          is_palindrome = false;
          break;
        }
      }
      if (is_palindrome) {
        cout << str << endl;
      }
    }
  }
  
  return 0;
}

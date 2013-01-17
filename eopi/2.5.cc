// Implement a method that takes as input a set S of distinct elements,
// and prints the power set of S (all subsets, incl empty set and S).
// Print subsets one per line, with elements separated by commas.

#include <iostream>
#include <vector>
#include <math.h>

using namespace std;

class Element {
public:
  Element(int x) : x_(x) { }
  int getX(void) const { return x_; }  
  friend ostream& operator<<(ostream& out, const Element& elem);
private:
  int x_;
};

ostream& operator<<(ostream& out, const Element& elem) {
  out << "(" << elem.getX() << ")";
  return out;
}

template <typename T>
void print_power_set(const vector<T>& S) {
  for (int i = 0; i < (1 << S.size()); i++) {
    int x = i;
    while (x) {
      int tar = log2(x ^ (x & (x - 1)));
      cout << S[tar];
      if (x &= x - 1) {
        cout << ',';
      }
    }
    cout << endl;
  }
}

int main(int argc, char *argv[]) {
  vector<Element> S;
  S.push_back(Element(3));
  S.push_back(Element(7));
  S.push_back(Element(9));
  print_power_set(S);
  return 0;
}

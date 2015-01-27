/*
ID: brooks.1
PROG: gift1
LANG: C++11
*/
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main() {
  ofstream fout("gift1.out");
  ifstream fin("gift1.in");

  int a, b;
  fin >> a >> b;
  fout << a+b << endl;

  return 0;
}

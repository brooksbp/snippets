/*
ID: brooks.1
PROG: ride
LANG: C++11
*/
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main() {
  ofstream fout("ride.out");
  ifstream fin("ride.in");

  string a; fin >> a;
  string b; fin >> b;

  int an = 1;
  for (auto c : a) { an *= c - 'A' + 1; }
  int bn = 1;
  for (auto c : b) { bn *= c - 'A' + 1; }

  if (an % 47 == bn % 47) {
    fout << "GO" << endl;
  } else {
    fout << "STAY" << endl;
  }

  return 0;
}

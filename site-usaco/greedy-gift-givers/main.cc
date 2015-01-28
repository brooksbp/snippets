/*
ID: brooks.1
PROG: gift1
LANG: C++11
*/
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <map>

using namespace std;

int main() {
  ifstream fin("gift1.in");
  ofstream fout("gift1.out");

  int NP; fin >> NP;

  vector<string> N(NP);
  map<string, int> M;

  for (int i = 0; i < NP; i++) {
    string s; fin >> s;
    N[i] = s;
    M[s] = 0;
  }

  string giver;
  while (fin >> giver) {
    int d; fin >> d;  // initial amount of money
    int n; fin >> n;  // number of people whom the giver will give gifts

    if (n < 1) continue;

    int d_per_person = d / n;
    int d_leftover = d % n;

    for (int i = 0; i < n; i++) {
      string s; fin >> s;
      M[s] += d_per_person;
    }
    M[giver] -= d;
    M[giver] += d_leftover;
  }

  for (auto& n : N) { fout << n << " " << M[n] << endl; }

  return 0;
}

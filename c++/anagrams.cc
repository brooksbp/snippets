#include <iostream>
#include <vector>
#include <utility>
#include <unordered_map>
#include <limits>
#include <algorithm>

using namespace std;

int main() {
  vector<string> V;
  string s;
  while (cin >> s) V.push_back(s);

  for (auto& s : V) {
    sort(s.begin(), s.end());
  }
  unordered_map<string, int> D;
  for (auto& s : V) {
    if (D.find(s) == D.end()) {
      D[s] = 1;
    } else {
      D[s]++;
    }
  }
  int a = 0;
  for (auto kv : D) {
    if (kv.second > 1) {
      a += kv.second;
      cout << kv.first << " " << kv.second << endl;
    }
  }
  cout << a << endl;

  return 0;
}

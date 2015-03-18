#include <iostream>
#include <vector>
#include <utility>
#include <unordered_map>
#include <limits>

using namespace std;

int main() {
  vector<int> V;
  int v;
  while (cin >> v) V.push_back(v);

  unordered_map<int, pair<int, int>> D;
  int age = 0;
  for (auto e : V) {
    age++;
    auto p = D.find(e);
    if (p == D.end()) {
      D[e] = make_pair(1, age); // count, age
    } else {
      D[e].first++;
    }
  }

  int min_age = numeric_limits<int>::max();
  int min_e = -1;
  for (auto e : D) {
    auto p = e.second;
    if (p.first == 1 && p.second < min_age) {
      min_e = e.first;
      min_age = p.second;
      cout << "new elem: " << min_e << " age: " << min_age << endl;
    } else {
      cout << "skp elem: " << e.first << " age: " << p.second << endl;
    }
  }

  cout << "elem: " << min_e << " age: " << min_age << endl;

  return 0;
}

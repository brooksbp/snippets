#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_set>

using namespace std;

void remove_duplicates_sorted(vector<int>* v) {
  //sort(v->begin(), v->end());
  size_t i = 0;
  for (size_t j = 1; j < v->size(); j++) {
    if ((*v)[i] != (*v)[j]) {
      i++;
      (*v)[i] = (*v)[j];
    }
  }
  v->resize(i+1);
}

void remove_duplicates(vector<int>* v) {
  if (v->size() < 2) return;

  unordered_set<int> s;
  size_t i = 0;

  for (size_t j = 0; j < v->size(); j++) {
    int e = (*v)[j];

    if (s.count(e) == 0) {
      (*v)[i++] = e;
      s.insert(e);
    }
  }
  v->resize(i);
}

int main() {
  vector<int> v = { 1, 1, 2, 3, 3 };
  for (auto e : v) cout << e << " "; cout << endl;
  remove_duplicates_sorted(&v);
  for (auto e : v) cout << e << " "; cout << endl;

  vector<int> z = { 9, 3, 5, 8, 2, 4, 7, 4, 6, 7, 4, 3 };
  for (auto e : z) cout << e << " "; cout << endl;
  remove_duplicates(&z);
  for (auto e : z) cout << e << " "; cout << endl;

  return 0;
}

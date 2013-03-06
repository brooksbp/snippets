// http://stackoverflow.com/questions/15132082/finding-set-of-pairs-that-correspond-to-list-of-sums/15238831#15238831

#include <vector>
#include <iostream>
#include <algorithm>
#include <utility>

using namespace std;

// numerical 3d match: x + y + z = b where
// x = a, y = b, z = -c, b = 0
template <typename T>
vector<pair<vector<T>, vector<T> > > n3dmatch(vector<T> a, vector<T> b, vector<T> c) {
  vector<pair<vector<T>, vector<T> > > result;
  if (a.size() != b.size() || b.size() != c.size()) return result;

  vector<vector<T> > ap, bp;
  sort(a.begin(), a.end());
  sort(b.begin(), b.end());
  do { ap.push_back(a); } while (next_permutation(a.begin(), a.end()));
  do { bp.push_back(b); } while (next_permutation(b.begin(), b.end()));

  for (int i = 0; i < ap.size(); i++) {
    for (int j = 0; j < ap.size(); j++) {
      bool match = true;
      for (int k = 0; k < a.size(); k++) {
        if ((ap[i][k] + bp[j][k]) != c[k]) {
          match = false; break;
        }
      }
      if (match) result.push_back({ ap[i], bp[j] });
    }
  }  
  return result;
}

int main(int argc, char *argv[]) {
  vector<int> a = { 1, 2, 3 };
  vector<int> b = { 4, 5, 6 };
  vector<int> c = { 6, 7, 8 };
  //vector<int> c = { 7, 7, 7 };
  auto result = n3dmatch(a, b, c);
  for (int i = 0; i < result.size(); i++) {
    vector<int> &a = result[i].first;
    vector<int> &b = result[i].second;
    for (int j = 0; j < a.size(); j++) cout << a[j] << " "; cout << endl;    
    for (int j = 0; j < b.size(); j++) cout << b[j] << " "; cout << endl;
    cout << "-" << endl;
  }
  return 0;
}

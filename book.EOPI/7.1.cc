#include <iostream>
#include <vector>
#include <utility>
#include <queue>

using namespace std;

// Merge 500 sorted files into a 5GB file.
// Use a min-heap containing 500 elements.
// Insert O(logn)
// min O(1)
// delete-min O(logn) ?
// logn better than n linear search!

template <typename T>
class Compare {
 public:
  bool operator()(const pair<T, int> &lhs, const pair<T, int> &rhs) const {
    return lhs.first > rhs.first;
  }
};

template <typename T>
vector<T> merge_arrays(const vector<vector<T> > &S) {
  priority_queue<pair<T, int>, vector<pair<T, int> >, Compare<T> > min_heap;
  vector<int> S_idx(S.size(), 0);

  for (int i = 0; i < S.size(); i++) {
    if (S[i].size() > 0) {
      min_heap.emplace(S[i][0], i);
      S_idx[i] = 1;
    }
  }
  
  vector<T> ret;
  while (!min_heap.empty()) {
    pair<T, int> p = min_heap.top();
    ret.emplace_back(p.first);

    if (S_idx[p.second] < S[p.second].size()) {
      min_heap.emplace(S[p.second][S_idx[p.second]++], p.second);
    }
    min_heap.pop();
  }
  return ret;
}

int main(int argc, char *argv[]) {

  vector<int> a = { 0, 1, 7, 9 };
  vector<int> b = { 2, 8, 70, 90, 111, 1000 };

  vector<vector<int> > z = { a, b };

  vector<int> result = merge_arrays(z);
  for (auto it = result.begin(); it != result.end(); it++) {
    cout << *it << " ";
  }
  cout << endl;
  
  return 0;
}

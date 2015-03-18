#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
#include <limits>
using namespace std;

long long go_contig(const vector<int>& A) {
  long long min_sum = 0;
  long long sum = 0, max_sum = 0;

  for (int i = 0; i < A.size(); i++) {
    sum += A[i];

    if (sum < min_sum) {
      min_sum = sum;
    }
    if (sum - min_sum > max_sum) {
      max_sum = sum - min_sum;
    }
  }
  if (max_sum == 0) {
    return *max_element(A.begin(), A.end());
  } else  {
    return max_sum;
  }
}
long long go_non_contig(const vector<int>& A) {
  unsigned long long result = 0;
  int max_negative = std::numeric_limits<int>::min();
  bool all_negative = true;
  for (auto e : A) {
    if (e > 0) {
      result += e;
      all_negative = false;
    } else {
      if (e > max_negative) max_negative = e;
    }
  }
  return (all_negative) ? max_negative : result;
}

int main() {
  int T; cin >> T;
  while (T--) {
    int N; cin >> N;

    vector<int> A(N);
    for (int i = 0; i < N; i++) cin >> A[i];

    cout << go_contig(A) << " " << go_non_contig(A) << endl;
  }
  return 0;
}

#include <iostream>
#include <vector>
#include <utility>
#include <cmath>

using namespace std;

#define max3(a_, b_, c_) max(max((a_), (b_)), (c_))
#define min3(a_, b_, c_) min(min((a_), (b_)), (c_))

pair<int, int> ClosestTriplet(const vector<vector<int> > &A) {
  pair<int, int> ans = { -1, -1 };
  const vector<int>& I = A[0];
  const vector<int>& J = A[1];
  const vector<int>& K = A[2];

  for (size_t i = 0; i < I.size(); i++) {
    for (size_t j = 0; j < J.size(); j++) {
      for (size_t k = 0; k < K.size(); k++) {
        int ii = I[i];
        int jj = J[j];
        int kk = K[k];
        int min = min3(ii, jj, kk);
        int max = max3(ii, jj, kk);
        if (abs(max3(I[i], J[j], K[k])))
          // todo
      }
    }
  }

  return ans;
}

int main(void) {
  vector<vector<int> > A = {
    { 1, 1000, 2000 },
    { 20, 10001, 50000 },
    { 55, 10002, 222222 }
  };

  
  return 0;
}

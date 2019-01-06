#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

int main() {
  int N; cin>>N;  // N jars
  int M; cin>>M;  // M operations

  vector<double> A(N, 0.0);
  int K = 0;

  while (K < M) {
    const int C = 2140;
    vector<int> V(N, 0);

    int c;
    for (c = 0; c < C && K < M; c++, K++) {
      int a; cin>>a; a--;
      int b; cin>>b; b--;
      int k; cin>>k;
      for (int j = a; j <= b; j++) V[j] += k;
    }

    for (int j = 0; j < N; j++) {
      A[j] = ((A[j] * (K-c)) + (double)V[j]) / K;
    }
  }
#if 0
  vector<double> A(N, 0.0);
  int K = 0;

  while (K < M) {
    int a; cin>>a; a--;
    int b; cin>>b; b--;
    int kk; cin>>kk;

    for (int j = 0; j < N; j++) {
      int k = (j >= a && j <= b) ? kk : 0;
      A[j] = ((A[j] * K) + k) / (K+1);
    }
    K++;
  }
#endif

  double avg = 0.0;
  for (int i = 0; i < N; i++) avg += A[i] / N;
  long long ans = floor(avg * M);
  cout << ans << endl;

  return 0;
}

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  int T; cin>>T;

  while (T--) {
    int N; cin>>N;
    int K; cin>>K;

    vector<int> A(N);
    for (int i = 0; i < N; i++) cin>>A[i];
    vector<int> B(N);
    for (int i = 0; i < N; i++) cin>>B[i];

    sort(A.begin(), A.end());
    sort(B.begin(), B.end());
    reverse(B.begin(), B.end());

    bool ans = true;
    for (int i = 0; i < N; i++) {
      if (A[i]+B[i] < K) {
        ans = false;
        break;
      }
    }

    cout << (ans ? "YES" : "NO") << endl;
  }

  return 0;
}

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Quick Union with path compression.
int QU_root(vector<int>& A, int i) {
  while (i != A[i]) {
    A[i] = A[A[i]];
    i = A[i];
  }
  return i;
}
int QU_find(vector<int>& A, int p, int q) {
  return QU_root(A, p) == QU_root(A, q);
}
void QU_union(vector<int>& A, int p, int q) {
  int i = QU_root(A, p);
  int j = QU_root(A, q);
  A[i] = j;
}

int main() {
  int N; cin>>N; // num astronauts
  int I; cin>>I;

  // Each astronaut belongs to its own country
  vector<int> A(N);
  for (int i = 0; i < N; i++) A[i] = i;

  while (I--) {
    int a; cin>>a;
    int b; cin>>b;

    QU_union(A, min(a,b), max(a,b));
  }

  vector<int> C(N);
  for (int i = 0; i < N; i++) C[i] = 0;
  for (int i = 0; i < N; i++) C[QU_root(A, i)]++;

  // Number of permissible ways to choose a pair of asronauts from
  // different countries.
  long long ans = 0;
  for (int i = 0; i < N; i++) {
    if (C[i] == 0) continue;
    ans += C[i] * (N-C[i]);
  }
  ans >>= 1;
  // for (int i = 0; i < N-1; i++) {
  //   if (C[i] == 0) continue;
  //   for (int j = i+1; j < N; j++) {
  //     if (C[j] == 0) continue;
  //     ans += (C[i] * C[j]);
  //   }
  // }

  cout << ans << endl;

  return 0;
}

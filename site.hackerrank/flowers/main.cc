#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  int N; cin>>N;  // want to buy N flowers
  int K; cin>>K;  // K people

  vector<int> C(N);
  for (int i = 0; i < N; i++) cin>>C[i];

  sort(C.begin(), C.end());

  long long cost = 0;
  int f = 0;  // which friend we're on
  int m = 1;  // flower multiplier

  for (int i = N-1; i >= 0; i--) {
    cost += m * C[i];
    if (++f >= K) {
      f = 0;
      m++;
    }
  }

  cout << cost << endl;

#if 0
  vector<int> F(K, 0);  // how much each friend spends
  int Ci = 0;           // next cheapest flower

  for (int f = 0; f < F.size(); f++) {
    // max num flowers f _could_ buy while still having other friends buy one
    int nmax = N - K + 1;

    // try to buy from 1 up to nmax flows as long as at each step, the
    // total cost wont exceed the price of the next expensive flower.
    for (int n = 0; n < nmax; n++) {
      int flowers_bought = n+1;

      int cost = 0;
      for (int i = flowers_bought, j = 0; i > 0; i--, j++) {
        cost += i * C[Ci + j];
      }

      if (flowers_bought == nmax || cost > C[Ci+flowers_bought]) {
        F[f] = cost;
        Ci += flowers_bought;
        break;
      }
    }
  }

  long long ans = 0;
  for (auto f : F) ans += f;
  cout << ans << endl;
#endif

  return 0;
}

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  int T; cin >> T;
  while (T--) {
    int N; cin >> N;
    int K; cin >> K;

    int ot = 0;
    while (N--) {
      int a; cin >> a;
      if (a < 1) ot++;
    }
    if (ot >= K)
      cout << "NO" << endl;
    else
      cout << "YES" << endl;
  }
  return 0;
}

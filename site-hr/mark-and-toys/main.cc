#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  int N; cin>>N;
  int K; cin>>K;

  vector<int> P(N);
  for (int i = 0; i < N; i++) cin>>P[i];

  sort(P.begin(), P.end());

  int count = 0;
  while (K > 0 && K >= P[count]) {
    K -= P[count];
    count++;
  }

  cout << count << endl;

  return 0;
}

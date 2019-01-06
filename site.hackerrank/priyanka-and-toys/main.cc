#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
  int N; cin>>N;

  vector<int> W(N);
  for (int i = 0; i < N; i++) cin>>W[i];

  sort(W.begin(), W.end());

  int units = 0;

  for (int i = 0; i < N; i++) {
    int wmax = W[i]+4;
    int ii = i;
    while (i < N && W[i] <= wmax) i++;
    if (ii != i) i--;
    units++;
  }

  cout << units << endl;

  return 0;
}

#include <vector>
#include <iostream>

using namespace std;

int factorial(int x) {
  return (x <= 1) ? 1 : x * factorial(x - 1);
}

int f(int a, int b) {
  return factorial(a + b) / (factorial(a) * factorial(b));
}

template <typename T>
int n(vector<T>& P) {
  if (P.size() <= 1) return 1;
  vector<T> L, R;
  for (int i = 1; i < P.size(); i++) {
    if (P[i] < P[0])
      L.push_back(P[i]);
    else
      R.push_back(P[i]);                         
  }
  return n(L) * n(R) * f(L.size(), R.size());
}

int main(int argc, char *argv[]) {
  vector<int> a = { 10, 5, 7, 20, 15, 30 };
  cout << n(a) << endl;
  return 0;
}

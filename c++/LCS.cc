#include <iostream>
#include <vector>

using namespace std;

enum { UP, LEFT, UPLEFT };

void lcs(const vector<int>& X, const vector<int>& Y,
         vector<vector<int>>& c, vector<vector<int>>& b) {
  size_t m = X.size();
  size_t n = Y.size();
  for (size_t i = 1; i <= m; i++) c[i][0] = 0;
  for (size_t j = 0; j <= n; j++) c[0][j] = 0;

  for (size_t i = 1; i <= m; i++) {
    for (size_t j = 1; j <= n; j++) {
      if (X[i-1] == Y[j-1]) {
        c[i][j] = c[i-1][j-1] + 1;
        b[i][j] = UPLEFT;
      } else {
        if (c[i-1][j] >= c[i][j-1]) {
          c[i][j] = c[i-1][j];
          b[i][j] = UP;
        } else {
          c[i][j] = c[i][j-1];
          b[i][j] = LEFT;
        }
      }
    }
  }
}

void print_lcs(const vector<vector<int>>& b, const vector<int>& X,
               const size_t i, const size_t j) {
  if (i == 0 || j == 0) return;
  if (b[i][j] == UPLEFT) {
    print_lcs(b, X, i-1, j-1);
    cout << X[i-1] << " ";
  } else if (b[i][j] == UP) {
    print_lcs(b, X, i-1, j);
  } else {
    print_lcs(b, X, i, j-1);
  }
}

#if 0
void lcs(const vector<int>& X, const vector<int>& Y, vector<vector<int>>& c) {
  size_t m = X.size();
  size_t n = X.size();
  for (size_t i = 1; i <= m; i++) c[i][0] = 0;
  for (size_t j = 0; j <= n; j++) c[0][j] = 0;

  for (size_t i = 1; i <= m; i++) {
    for (size_t j = 1; j <= n; j++) {
      if (X[i-1] == Y[j-1]) {
        c[i][j] = c[i-1][j-1] + 1;
      } else {
        if (c[i-1][j] >= c[i][j-1]) {
          c[i][j] = c[i-1][j];
        } else {
          c[i][j] = c[i][j-1];
        }
      }
    }
  }
}
void print_lcs(const vector<vector<int>>& c, const vector<int>& X,
               const size_t i, const size_t j) {
  if (i == 0 || j == 0) return;
  // TODO
}
#endif

int main() {
  vector<int> X = {1,2,3,2,4,1,2};
  vector<int> Y = {2,4,3,1,2,1};

  vector<vector<int>> c(X.size() +1, vector<int>(Y.size() +1));
  vector<vector<int>> b(X.size() +1, vector<int>(Y.size() +1));

  lcs(X, Y, c, b);

  print_lcs(b, X, X.size(), Y.size());
  cout << endl;

#if 0
  vector<vector<int>> c(X.size() +1, vector<int>(Y.size() +1));

  lcs(X, Y, c);

  print_lcs(c, X, X.size(), Y.size());
  cout << endl;
#endif
  return 0;
}

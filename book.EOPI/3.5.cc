#include <iostream>
#include <vector>

using namespace std;

template <typename T>
void longest_increasing_subarray(const vector<T>& A) {
  int max_start = 0, max_end = 0;
  int start = 0, end = 0;

  for (int i = 1; i < A.size(); i++) {
    if (A[i] > A[i - 1]) {
      end = i;
    } else {
      if ((max_end - max_start) < (end - start)) {
        max_end = end;
        max_start = start;
      }
      start = i;
      end = i;
    }
  }

  cout << max_start << " " << max_end << endl;  
}

// Derived O(n) solution on own. TODO: code up the O(n/L)
// solution which skips L and checks backwards!

int main(int argc, char *argv[]) {
  vector<int> A = { 0, 1, 2, 0, 1, 0, 1, 2, 3, 2 };
  longest_increasing_subarray(A);
  return 0;
}

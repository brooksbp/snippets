#include <algorithm>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <assert.h>

using namespace std;

bool Find2SumSorted_Slow(const vector<int>& numbers, int target, int* a, int* b) {
  bool found = false;
  size_t i, j = 0;
  for (i = 0; i < numbers.size() - 1; i++) {
    int rem = target - numbers[i];
    for (j = i + 1; j < numbers.size() && numbers[j] <= rem; j++) {
      if (numbers[j] == rem) {
        found = true; break;
      }
    }
    if (found) break;
  }
  if (found) { *a = i+1; *b = j+1; }
  return found;
}

bool Find2SumSorted(const vector<int>& numbers, int target, int* a, int* b) {
  size_t i = 0, j = numbers.size() - 1;

  while (numbers[i] + numbers[j] > target && i < j) {
    // Advance the index which yeilds the smallest delta.
    int a_delta = numbers[i+1] - numbers[i];
    int b_delta = numbers[j] - numbers[j-1];
    if (a_delta < b_delta)
      i++;
    else
      j--;
  }
  if (i < j && numbers[i] + numbers[j] == target) {
    *a = i+1;
    *b = j+1;
    return true;
  }
  return false;
}

bool Find2Sum(const vector<int>& numbers, int target, int* a, int* b) {
  unordered_map<int, size_t> H;
  H.reserve(numbers.size());

  for (size_t i = 0; i < numbers.size(); i++) {
    H[numbers[i]] = i;
  }
  for (auto i = H.begin(); i != H.end(); i++) {
    auto j = H.find(target - i->first);
    if (j != H.end()) {
      *a = min(i->second, j->second) + 1;
      *b = max(i->second, j->second) + 1;
      return true;
    }
  }
  return false;
}

int main() {

  int a, b;
  vector<int> v1 = { 2, 7, 11, 15 };

  assert(Find2SumSorted_Slow(v1, 9, &a, &b) == true && a == 1 && b == 2);
  assert(Find2SumSorted(v1, 9, &a, &b)      == true && a == 1 && b == 2);
  assert(Find2Sum(v1, 9, &a, &b)            == true && a == 1 && b == 2);

  return 0;
}

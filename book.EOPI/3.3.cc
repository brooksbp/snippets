#include <iostream>
#include <vector>
#include <limits>
#include <algorithm>

using namespace std;

template <typename HeightType>
HeightType battery_capacity(const vector<HeightType>& heights) {
  HeightType min_height = numeric_limits<HeightType>::max(), capacity = 0;
  for (const HeightType& height : heights) {
    capacity = max(capacity, height - min_height);
    min_height = min(min_height, height);
  }
  return capacity;
}

int main(int argc, char *argv[]) {
  vector<int> heights = { 0, 3, 2, 6 };
  cout << battery_capacity(heights) << endl;
  vector<int> heights2 = { 0, 1, -6, -3, -20 };
  cout << battery_capacity(heights2) << endl;
  return 0;
}

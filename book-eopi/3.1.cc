// Write a function that takes an array A and an index i into A, and
// rearranges the elements such that all elements less than A[i]
// appear first, followed by elements equal to A[i], followed by
// elements greater than A[i]. Your algorithm should have O(1) space
// complexity and O(|A|) time complexity.

#include <algorithm>
#include <iostream>

using namespace std;

template <typename T>
void dutch_flag_partition(vector<T> &A, const int &pivot_index) {
  int smaller = 0, equal = 0, larger = A.size() - 1;
  while (equal <= larger) {
    // cout << "s: " << smaller << " ";
    // cout << "e: " << equal << " ";
    // cout << "l: " << larger << endl;    
    if (A[equal] < A[pivot_index]) {
      swap(A[smaller++], A[equal++]);
    } else if (A[equal] == A[pivot_index]) {
      ++equal;
    } else {
      swap(A[equal], A[larger--]);
    }
  }
}

template <typename T>
void print_vector(vector<T> &v) {
  for (typename vector<T>::iterator i = v.begin(); i != v.end(); i++)
    cout << *i << " ";
  cout << endl;
}

int main(int argc, char *argv[]) {
  vector<int> a = { 0, 0, 2, 0, 1, 2, 0, 0, 2, 2 };
  dutch_flag_partition(a, 4);
  print_vector(a);

  // Note, in the problem description, we can assume that A[i]
  // can't be moved.  So, the pivot needs to be chosen carefully
  // so that it wont be moved and things are sorted correctly
  // (enough room on either side of pivot). The below is an
  // example of a bad test case.
  vector<int> b = { 0, 1, 2, 0, 1, 2, 0, 0, 2, 2 };
  dutch_flag_partition(b, 1);
  print_vector(b);
  // This raises the question of what about a variant where the
  // pivot can be moved, you just have to keep track of it..?
  
  return 0;
}

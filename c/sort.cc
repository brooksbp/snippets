#include <cmath>
#include <cstdlib>
#include <iostream>
#include <limits.h>
#include <stdint.h>
#include <sys/time.h>
#include <vector>

int64_t get_micros() {
  timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

template <typename V>
std::vector<V> GenerateVector(int n, int max_value = 1000000) {
  std::vector<V> v;
  for (int i = 0; i < n; i++) {
    v.push_back(rand() % (max_value + 1));
  }
  return v;
}

template <typename T>
void insertion_sort(std::vector<T> &A) {
  for (int j = 1; j < A.size(); j++) {
    T key = A[j];
    int i = j - 1;
    while (i >= 0 && A[i] > key) {
      A[i + 1] = A[i];
      i -= 1;
    }
    A[i + 1] = key;
  }
}

void run_insertion_sort(int n) {
  std::cout << n << "\t\t";
  std::vector<int> A = GenerateVector<int>(n);
  int64_t S = get_micros();
  insertion_sort(A);
  std::cout << (get_micros() - S) << std::endl;
}

template <typename T>
void merge(std::vector<T> &A, int p, int q, int r) {
  int n1 = q - p + 1;
  int n2 = r - q;
  std::vector<T> L, R;
  for (int i = 0; i < n1; i++) L.push_back(A[p + i]);
  for (int i = 0; i < n2; i++) R.push_back(A[q + i + 1]);
  L.push_back(INT_MAX);
  R.push_back(INT_MAX);
  int i = 0;
  int j = 0;
  for (int k = p; k <= r; k++) {
    if (L[i] <= R[j]) {
      A[k] = L[i];
      i += 1;
    } else {
      A[k] = R[j];
      j += 1;
    }
  }
}

template <typename T>
void __merge_sort(std::vector<T> &A, int p, int r) {
  if (p < r) {
    int q = floor((p + r) / 2);
    __merge_sort(A, p, q);
    __merge_sort(A, q + 1, r);
    merge(A, p, q, r);
  }
}

template <typename T>
void merge_sort(std::vector<T> &A) {
  if (A.size() > 1) {
    __merge_sort(A, 0, A.size() - 1);
  }
}

void run_merge_sort(int n) {
  std::cout << n << "\t\t";
  std::vector<int> A = GenerateVector<int>(n);
  int64_t S = get_micros();
  merge_sort(A);
  std::cout << (get_micros() - S) << std::endl;
}

int main(int argc, char *argv[]) {
  srand(time(NULL));
  run_insertion_sort(10);
  run_insertion_sort(100);
  run_insertion_sort(1000);
  run_insertion_sort(10000);
  run_insertion_sort(20000);
  run_merge_sort(10);
  run_merge_sort(100);
  run_merge_sort(1000);
  run_merge_sort(10000);
  run_merge_sort(100000);
  return 0;
}

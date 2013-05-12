#include <algorithm>
#include <future>
#include <list>
#include <utility>

#include <stdlib.h>
#include <time.h>

template<typename T>
std::list<T> sequential_quick_sort(std::list<T> input) {
  if (input.empty()) {
    return input;
  }
  std::list<T> result;
  result.splice(result.begin(), input, input.begin());
  T const& pivot = *result.begin();

  auto divide_point = std::partition(
      input.begin(), input.end(),
      [&] (T const& t){ return t < pivot; } );

  std::list<T> lower_part;
  lower_part.splice(lower_part.end(), input, input.begin(), divide_point);

  auto new_lower(sequential_quick_sort(std::move(lower_part)));
  auto new_higher(sequential_quick_sort(std::move(input)));

  result.splice(result.end(), new_higher);
  result.splice(result.begin(), new_lower);
  return result;
}

template<typename T>
std::list<T> parallel_quick_sort(std::list<T> input) {
  if (input.empty()) {
    return input;
  }
  std::list<T> result;
  result.splice(result.begin(), input, input.begin());
  T const& pivot = *result.begin();

  auto divide_point = std::partition(
      input.begin(), input.end(),
      [&] (T const& t){ return t < pivot; } );

  std::list<T> lower_part;
  lower_part.splice(lower_part.end(), input, input.begin(), divide_point);
  std::future<std::list<T> > new_lower(
      std::async(std::launch::async, &parallel_quick_sort<T>, std::move(lower_part)));

  auto new_higher(parallel_quick_sort(std::move(input)));

  result.splice(result.end(), new_higher);
  result.splice(result.begin(), new_lower.get());
  return result;
}

// Without passing std::launch::async, the parallel version is 4x slower
// than the sequential version.  htop shows only 1 cpu util and a short
// spike for all at the end.
// Passing std::launch::async grinds htop to a hault with all cores red
// and the proc is eventually killed.
// Counterintuitive results!!

int main(int argc, char *argv[]) {
  srand(time(NULL));
  
  std::list<int> l;
  for (int i = 0; i < 10000000; i++) {
    l.push_back(rand());
  }
  std::list<int> l2;
  for (int i = 0; i < 10000000; i++) {
    l2.push_back(rand());
  }

  
  std::list<int> sorted;
  time_t t1; time(&t1);
  sorted = sequential_quick_sort(l);
  time_t t2; time(&t2); printf("%ld\n", t2 - t1);
  sorted = parallel_quick_sort(l2);
  time_t t3; time(&t3); printf("%ld\n", t3 - t2);
  
  return 0;
}

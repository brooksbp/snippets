// Write a function that takes a single positive integer argument n (n > 2)
// and return all the primes between 1 and n.

#include <vector>
#include <iostream>

using namespace std;

vector<int> all_primes(int n) {
  vector<int> primes;
  primes.push_back(2);

  for (int i = 3; i < n+1; i++) {
    bool prime = true;
    for (int j = 0; j < primes.size() && primes[j] * primes[j] <= i; j++) {
      if (i % primes[j] == 0) { prime = false; break; }
    }
    if (prime) primes.push_back(i);
  }

  return primes;
}

int main(int argc, char *argv[]) {
  vector<int> r = all_primes(60);

  for (auto it = r.begin(); it != r.end(); it++)
    cout << *it << " ";
  cout << endl;
    
  return 0;
}

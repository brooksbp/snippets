// Let h1 and h2 be the heads of lists L1 and L2. Assume that L1
// and L2 are well-formed, finite sequence of nodes, no cycles.
// How would you determine if there exists a node r reachable
// from both h1 and h2 by following the next fields? If such a node
// exists, find the node that appears earliest when traversing the
// lists. You are constrained to use no more than constant
// additional storage.

#include "list.h"

// Traverse both one-by-one hashing addresses, but this is not constant space.

// O(n^2) solution where outer loop traverses L1 and inner loop traverses
// L2 searching for a match.

// Notice that if they merge, they must have the same tail node.

template <typename T>
int count_len(shared_ptr<node_t<T> > L) {
  int len = 0;
  while (L) {
    ++len; L = L->next;
  }
  return len;
}

template <typename T>
shared_ptr<node_t<T> > reachable_exists(
    shared_ptr<node_t<T> > L1, shared_ptr<node_t<T> > L2) {
  // Count the lengths of L1 and L2
  int L1_len = count_len<T>(L1), L2_len = count_len<T>(L2);

  while (L1_len > L2_len) {
    L1 = L1->next; --L1_len;
  }
  while (L2_len > L1_len) {
    L2 = L2->next; --L2_len;
  }
  while (L1 && L2 && L1 != L2) {
    L1 = L1->next; L2 = L2->next;
  }
  return L1;
}

int main(int argc, char *argv[]) {
  
  return 0;
}

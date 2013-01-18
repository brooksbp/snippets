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

int main(int argc, char *argv[]) {
  
  return 0;
}

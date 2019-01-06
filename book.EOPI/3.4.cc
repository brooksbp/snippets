// for each subset of A:
//   if sum(subset) % A.len == 0:
//     return subset

// Solution uses a prefix array and yeilds contiguous
// ranges of A as the answer.  Is not clear to me how
// it is possible to return a non-contiguous subset of
// A with this solution.  Queried authors on Quora.

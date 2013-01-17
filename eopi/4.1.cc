// Given a reference to the head of a singly linked list L, how
// would you determine whether L ends in a null or reaches a
// cycle of nodes? Write a function that returns null if there
// does not exist a cycle, and the reference to the start of the
// cycle if a cycle is present.

#include "list.h"

template <typename T>
shared_ptr<node_t<T> > has_cycle(const shared_ptr<node_t<T> > &head) {
  return NULL;
}

int main(int argc, char *argv[]) {  
  shared_ptr<node_t<int> > head;
  push(head, int(0));
  push(head, int(5));
  push(head, int(6));
  print_list(head);  
  return 0;
}

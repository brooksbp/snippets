#include "list.h"

template <typename T>
void even_odd_merge(shared_ptr<node_t<T> >& head) {
  bool odd = false;
  shared_ptr<node_t<T> > node = head;
  shared_ptr<node_t<T> > first_odd = head->next;
  shared_ptr<node_t<T> > next;
  while (node) {
    next = node->next;
    if (odd) {
      if (next != nullptr) node->next = next->next;
    } else {
      if (next == nullptr || next->next == nullptr) {
        node->next = first_odd;
        return;
      } else {
        node->next = next->next;
      }
    }
    odd = !odd;
    node = next;
  }
}

// A bit sloppy compared to eopi solution TODO

int main(int argc, char *argv[]) {
  shared_ptr<node_t<int> > h1;
  push(h1, int(6));
  push(h1, int(5));
  push(h1, int(4));
  push(h1, int(3));
  push(h1, int(2));
  push(h1, int(1));
  push(h1, int(0));
  print_list(h1);
  even_odd_merge(h1);
  print_list(h1);
  
  return 0;
}

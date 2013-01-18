// Write a function that takes a sorted circular singly linked list and a
// pointer to an arbitrary node in this linked list, and returns the median
// of the linked list.

#include "list.h"

// Note: assuming arithmetic operators defined for 'T'
template <typename T>
double find_median_sorted_cll(const shared_ptr<node_t<T> > &node) {
  if (!node) {
    return 0.0;
  }
  int length = 0;
  T min = node->data;
  shared_ptr<node_t<T> > pMin = node;
  shared_ptr<node_t<T> > tmp = node;
  do {
    if (tmp->data < min) {
      min = tmp->data;
      pMin = tmp;
    }
    length++;    
    tmp = tmp->next;
  } while (tmp && tmp != node);

  if (length == 1) {
    return node->data;
  }
  
  tmp = pMin;
  if (length % 2 == 0) {
    length = (length / 2) - 1;
    while (length--) tmp = tmp->next;
    return (tmp->data + tmp->next->data) / 2;
  } else {
    length /= 2;
    while (length--) tmp = tmp->next;
    return tmp->data;
  }
}   

int main(int argc, char *argv[]) {
  // Note: Not using circular linked lists here, but should be fine
  shared_ptr<node_t<int> > h1;
  push(h1, int(5));
  push(h1, int(4));
  push(h1, int(3));
  push(h1, int(2));
  push(h1, int(1));
  cout << find_median_sorted_cll(h1) << endl;

  shared_ptr<node_t<int> > h2;
  push(h2, int(5));
  cout << find_median_sorted_cll(h2) << endl;

  shared_ptr<node_t<int> > h3;
  push(h3, int(10));
  push(h3, int(4));
  cout << find_median_sorted_cll(h3) << endl;
  
  return 0;
}

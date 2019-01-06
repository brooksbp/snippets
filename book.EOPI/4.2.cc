// Write a function that takes a sorted circular singly linked list and a
// pointer to an arbitrary node in this linked list, and returns the median
// of the linked list.

#include "list.h"

// Note: assuming arithmetic operators defined for 'T'.
template <typename T>
double find_median_sorted_cll(const shared_ptr<node_t<T> > &node) {
  if (!node) {
    return 0.0;
  }
  // Find the min and length.
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
  } while (tmp != node);

  if (length == 1) {
    return node->data;
  }

  // Advance by 'half' and return median.
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

// Note: They claim "one corner case to watch out for is all entries
// being equal, which we check for in the first stage since we cannot
// find the first node with the minimum element."
template <typename T>
double find_median_sorted_circular_linked_list(
    const shared_ptr<node_t<T> > &r_node) {
  if (!r_node) {
    return 0.0; // no node in this linked list
  }

  // Check all nodes are identical or not,
  shared_ptr<node_t<T> > curr = r_node;
  int count = 0;
  bool is_identical = true;
  do {
    if (curr->data != curr->next->data) {
      is_identical = false;
    }
    ++count, curr = curr->next;
  } while (curr != r_node);

  // If all values are identical, median = curr->data
  if (is_identical == true) {
    return curr->data;
  }

  // Try to identify the start of this linked list
  // Note: this does not account for reverse sorted lists!!!
  shared_ptr<node_t<T> > head = r_node;
  while (head->data <= head->next->data) {
    head = head->next;
  }
  head = head->next;

  // Traverse to the middle of the list and return the median.
  for (int i = 0; i < (count - 1) >> 1; i++) {
    head = head->next;
  }
  return (count & 1) ? head->data : 0.5 * (head->data + head->next->data);
}

int main(int argc, char *argv[]) {
  // Note: Not using circular linked lists here, fine for my
  // implementation, but not fine for EOPI impl. Segfault when
  // trying to identify start of linked list, it wraps.
  shared_ptr<node_t<int> > h1;
  push(h1, int(5));
  push(h1, int(4));
  push(h1, int(3));
  push(h1, int(2));
  push(h1, int(1));
  cout << find_median_sorted_cll(h1) << endl;
  cout << find_median_sorted_circular_linked_list(h1) << endl;

  shared_ptr<node_t<int> > h2;
  push(h2, int(5));
  cout << find_median_sorted_cll(h2) << endl;
  cout << find_median_sorted_circular_linked_list(h2) << endl;

  shared_ptr<node_t<int> > h3;
  push(h3, int(10));
  push(h3, int(4));
  cout << find_median_sorted_cll(h3) << endl;
  cout << find_median_sorted_circular_linked_list(h3) << endl;
  
  return 0;
}

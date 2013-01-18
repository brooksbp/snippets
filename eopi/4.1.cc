// Given a reference to the head of a singly linked list L, how
// would you determine whether L ends in a null or reaches a
// cycle of nodes? Write a function that returns null if there
// does not exist a cycle, and the reference to the start of the
// cycle if a cycle is present.

#include "list.h"
#include <unordered_map>

// Traverse list storing addresses in hash table. If run across an
// address, there exists a cycle. O(n) space where n = number of nodes
template <typename T>
shared_ptr<node_t<T> > has_cycle(const shared_ptr<node_t<T> > &head) {
  unordered_map<shared_ptr<node_t<T> >, int> map;
  for (shared_ptr<node_t<T> > tmp = head; tmp; tmp = tmp->next) {
    map[tmp] += 1;
    if (1 < map[tmp]) {
      return tmp;
    }
  }  
  return nullptr;
}
// Instead of using a hash table, another approach is to mark the LSB
// of the next pointer when visited.

// Another alternative is to reverse the list. Should reach either the
// head or tail. Modifies list though.

// Use slow & fast iterators. If detected, start from head and distance
// slow and fast by the length of the cycle and increment by one until
// fast wraps the cycle to meet slow.
template <typename T>
shared_ptr<node_t<T> > has_cycle2(const shared_ptr<node_t<T> > &head) {
  shared_ptr<node_t<T> > slow = head, fast = head;

  while (slow && slow->next && fast && fast->next && fast->next->next) {
    slow = slow->next;
    fast = fast->next->next;

    if (slow == fast) { // cycle detected
      int cycle_len = 0;
      do {
        fast = fast->next;
        cycle_len++;
      } while (slow != fast);

      slow = head; fast = head;
      while (cycle_len--) {
        fast = fast->next;
      }

      while (slow != fast) {
        slow = slow->next;
        fast = fast->next;
      }
      return slow;
    }
  }
  return nullptr;
}

int main(int argc, char *argv[]) {  
  shared_ptr<node_t<int> > h1;
  push(h1, int(0));
  push(h1, int(5));
  push(h1, int(6));
  cout << has_cycle(h1) << endl;
  cout << has_cycle2(h1) << endl;

  shared_ptr<node_t<int> > h2 (new node_t<int>);
  h2->data = 0;
  h2->next = h2;
  cout << has_cycle(h2) << endl;
  cout << has_cycle2(h2) << endl;

  shared_ptr<node_t<int> > h3 (new node_t<int>);
  h3->data = 0;
  h3->next = make_shared<node_t<int> >();
  h3->next->data = 1;
  h3->next->next = h3;
  cout << has_cycle(h3) << endl;
  cout << has_cycle2(h3) << endl;

  return 0;
}

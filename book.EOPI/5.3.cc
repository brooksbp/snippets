// Given a BST node n, print all the keys at n and its descendants. The
// nodes should be printed in sorted order, and you cannot use recursion.

#include <iostream>
#include <memory>
#include <stack>

using namespace std;

template <typename T>
class node_t {
 public:
  ~node_t() { cout << "deleting: " << key << endl; }
  T key;
  shared_ptr<node_t<T> > left, right;
};

// iterative, in-order traversal
template <typename T>
void print_bst(const shared_ptr<node_t<T> > node) {
  stack<shared_ptr<node_t<T> > > s;  
  while (node || !s.empty()) {
    if (node) {
      s.push(node);
      node = node->left;
    } else {
      node = s.top(); s.pop();
      cout << node.key << endl;
      node = node->right;
    }
  }
}

int main(int argc, char *argv[]) {
  
  return 0;
}

#include <iostream>
#include "BinaryTree.h"

using namespace std;

// Recursive method takes O(h) space h=height due
// to call stack. Morris traversal is an in-order
// traversal that takes O(1) space. Note that it
// is not thread-safe since it modifies the tree.

template <typename T>
void morris_traversal(shared_ptr<BinaryTree<T> > n) {
  while (n) {
    if (n->left) {
      // Find the predecessor of n
      shared_ptr<BinaryTree<T> > pre = n->left;
      while (pre->right && pre->right != n) {
        pre = pre->right;
      }
      // Build the successor link
      if (pre->right) { // pre->right == n
        // Revert the successor link if predecessor's successor is n
        pre->right = nullptr;
        cout << n->data << endl;
        n = n->right;
      } else { // if predecessor's successor is not n
        pre->right = n;
        n = n->left;
      }
    } else {
      cout << n->data << endl;
      n = n->right;
    }
  }
}

int main(int argc, char *argv[]) {

  shared_ptr<BinaryTree<int> > a = make_shared<BinaryTree<int> >();
  a->data = 0;

  auto b = make_shared<BinaryTree<int> >();
  b->data = 1;
  a->left = b;

  auto c = make_shared<BinaryTree<int> >();
  c->data = 2;
  a->right = c;
  
  morris_traversal(a);
  
  return 0;
}

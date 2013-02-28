#include <iostream>
#include <utility>
#include <cmath>
#include "BinaryTree.h"

using namespace std;

// First impression was to do an interative pretraversal
// on left-subtree storing visited nodes in a list, then
// do iterative pretraversal where right then left and
// store nodes. O(n)

template <typename T>
bool sym(shared_ptr<BinaryTree<T> > l,
         shared_ptr<BinaryTree<T> > r) {
  if (!l && !r) return true;
  if (!l || !r) return false;
  return (l->data == r->data) &&
      sym(l->left, r->right) &&
      sym(l->right, r->left);
}

template <typename T>
bool symmetric(shared_ptr<BinaryTree<T> > n) {
  return !n || sym(n->left, n->right);
}

int main(int argc, char *argv[]) {

  shared_ptr<BinaryTree<int> > a = make_shared<BinaryTree<int> >();
  a->data = 0;

  auto b = make_shared<BinaryTree<int> >();
  b->data = 1;
  a->left = b;

  auto c = make_shared<BinaryTree<int> >();
  c->data = 2;
  //a->right = c;
  b->left = c;

  cout << symmetric(a) << endl;
  return 0;
}

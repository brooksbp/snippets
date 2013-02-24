#include <iostream>
#include <limits>
#include <cmath>
#include "BinaryTree.h"

using namespace std;

// balanced if for every node, the difference in height
// of left and right subtrees are at most 1

template <typename T>
int balanced_height(shared_ptr<BinaryTree<T> > n) {
  if (!n) return 0;
  if (!n->left && !n->right) return 1;

  int hL = balanced_height(n->left);
  int hR = balanced_height(n->right);

  if (abs(hL - hR) > 1) return numeric_limits<int>::max();
  else return 1 + max(hL, hR);
}

template <typename T>
bool is_balanced(shared_ptr<BinaryTree<T> > n) {
  return balanced_height(n) != numeric_limits<int>::max();
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

  cout << is_balanced(a) << endl;
  
  return 0;
}

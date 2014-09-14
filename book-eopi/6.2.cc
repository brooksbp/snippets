#include <iostream>
#include <utility>
#include <cmath>
#include "BinaryTree.h"

using namespace std;

template <typename T>
pair<shared_ptr<BinaryTree<T> >, int > find_non_k_balanced(
    shared_ptr<BinaryTree<T> > n,
    int k) {
  if (!n) return {nullptr, 0};

  auto L = find_non_k_balanced(n->left, k);
  if (L.first) {
    return L;
  }
  auto R = find_non_k_balanced(n->right, k);
  if (R.first) {
    return R;
  }

  shared_ptr<BinaryTree<T> > node = nullptr;
  if (abs(L.second - R.second) > k) {
    node = n;
  }
  return {node, 1 + L.second + R.second};
}

template <typename T>
shared_ptr<BinaryTree<T> > k_balanced(
    shared_ptr<BinaryTree<T> >& n,
    int k) {
  return find_non_k_balanced<T>(n, k).first;
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

  auto n = k_balanced(a, 0);
  if (n) {
    cout << n->data << endl;
  }
  return 0;
}

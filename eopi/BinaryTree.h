#include <memory>

using namespace std;

template <typename T>
class BinaryTree {
 public:
  T data;
  shared_ptr<BinaryTree<T> > left, right;
};

template <typename T>
void print(shared_ptr<BinaryTree<T> > n) {
  if (!n) return;
  print(n->left);
  cout << n->data << endl;
  print(n->right);
}

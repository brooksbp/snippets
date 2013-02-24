#include <memory>

using namespace std;

template <typename T>
class BinaryTree {
 public:
  T data;
  shared_ptr<BinaryTree<T> > left, right;
};

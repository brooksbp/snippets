#include <iostream>

using namespace std;


template <typename T>
class node_t {
 public:
  T data;
  shared_ptr<node_t<T> > next;
};

int main(int argc, char *argv[]) {
  
  return 0;
}

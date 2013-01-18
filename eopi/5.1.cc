// Design a stack that supports a max operation, which returns the maximum
// value stored in the stack, and throws an exception if the stack is empty.
// Assume elements are comparable. All operations must be O(1). You can use
// O(n) additional space beyond what is required for the elements themselves.

#include <stack>
#include <exception>

using namespace std;

template <typename T>
class stack_t {
  stack<pair<T, T> > s;
  
 public:
  void push(T &elem) {
    if (s.empty()) {
      s.push(make_pair(elem, elem));
    } else {
      T &max = s.top().second;
      s.push(make_pair(elem, (elem > max) ? elem : max));
    }
  }
  T pop(void) {
    if (s.empty()) throw "stack empty!";
    T elem = s.top().first;
    s.pop();
    return elem;
  }
  T max(void) {
    if (s.empty()) throw "stack empty!";
    return s.top().second;
  }
};

int main(int argc, char *argv[]) {
  
  return 0;
}

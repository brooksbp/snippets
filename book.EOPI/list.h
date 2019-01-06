#include <iostream>
#include <memory>

using namespace std;

template <typename T>
class node_t {
 public:
  ~node_t() { cout << "deleting: " << data << endl; }
  T data;
  shared_ptr<node_t<T> > next;
};

template <typename T>
void print_list(const shared_ptr<node_t<T> > &head) {
  for (shared_ptr<node_t<T> > tmp = head; tmp; tmp = tmp->next) {
    cout << "data: " << tmp->data << " next.use_count(): " << tmp->next.use_count() << endl;
  }
}

template <typename T>
void push(shared_ptr<node_t<T> > &head, T data) {
  shared_ptr<node_t<T> > tmp (new node_t<T>);
  tmp->data = data;
  tmp->next = head;
  head = tmp;
}

#include <list>
#include <mutex>
#include <algorithm>

std::list<int> some_list;
std::mutext some_mutext;

void add_to_list(int new_value) {
  std::lock_guard<std::mutex> guard(some_mutex);
  some_list.push_back(new_value);
}
bool list_contains(int value_to_find) {
  std::lock_guard<std::mutex> guard(some_mutex);
  return std::find(some_list.begin(),
                   some_list.end(),
                   value_to_find) != some_list.end();
}


// Structuring code to protect shared data

class some_data {
  int a;
  std::string b;
 public:
  void do_something();
};

class data_wrapper {
 private:
  some_data data;
  std::mutex m;
 public:
  template<typename Function>
  void process_data(Function func) {
    std::lock_guard<std::mutex> l(m);
    func(data);
  }
};

some_data *unprotected;

void malicious_fn(some_data& protected_data) {
  unprotected = &protected_data;
}

data_wrapper x;

void foo() {
  x.process_data(malicious_fn);
  unprotected->do_something();
}

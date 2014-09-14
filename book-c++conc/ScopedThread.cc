#include <thread>

class scoped_thread {
  std::thread t;
 public:
  explicit scoped_thread(std::thread t_)
      : t(std::move(t_)) {
    if (!t.joinable()) {
      throw std::logic_error("No thread");
    }
  }
  ~scoped_thread() {
    t.join();
  }
  scoped_thread(scoped_thread const&)=delete;
  scoped_thread& operator=(scoped_thread const&)=delete;
};

struct func;

void f() {
  int some_local_state;
  scoped_thread t(std::thread(func(some_local_state)));

  do_something_in_current_thread();
}

// Move-aware std containers:

void do_work(unsigned id);

void f() {
  std::vector<std::thread> threads;
  for (int i = 0; i < 20; i++) {
    threads.push_back(std::thread(do_work, i)); // spawn threads
  }
  std::for_each(threads.begin(); threads.end(),
                std::mem_fn(&std::thread::join));
}

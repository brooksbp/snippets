#include <algorithm>
#include <atomic>
#include <iostream>
#include <thread>
#include <mutex>
#include <vector>

class spinlock_mutex {
  std::atomic<bool> flag;
 public:
  spinlock_mutex() : flag(false) {
  }
  void lock() {
    bool expected;
    do {
      expected = false;
    } while (!flag.compare_exchange_weak(expected, true));
  }
  void unlock() {
    flag.store(false);
  }
  bool is_lock_free() {
    return flag.is_lock_free();
  }
};

spinlock_mutex m;

void thread_main(int id) {
  std::lock_guard<spinlock_mutex> lk(m);
  std::cout << "thread " << id << std::endl;
}

int main(int argc, char *argv[]) {
  std::vector<std::thread> threads;
  
  std::cout << "m.is_lock_free() = " << m.is_lock_free() << std::endl;
  
  for (int i = 0; i < 400; i++) {
    threads.push_back(std::thread(thread_main, i));
  }
  std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
  return 0;
}

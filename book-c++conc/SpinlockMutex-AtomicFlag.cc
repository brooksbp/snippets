#include <algorithm>
#include <atomic>
#include <iostream>
#include <thread>
#include <mutex>
#include <vector>

class spinlock_mutex {
  std::atomic_flag flag;
 public:
  spinlock_mutex() : flag(ATOMIC_FLAG_INIT) {
  }
  void lock() {
    while (flag.test_and_set(std::memory_order_acquire)) ;
  }
  void unlock() {
    flag.clear(std::memory_order_release);
  }
};

spinlock_mutex m;

void thread_main(int id) {
  std::lock_guard<spinlock_mutex> lk(m);
  //m.lock();
  std::cout << "thread " << id << std::endl;
  //m.unlock();
}

int main(int argc, char *argv[]) {
  std::vector<std::thread> threads;
  for (int i = 0; i < 4; i++) {
    threads.push_back(std::thread(thread_main, i));
  }
  std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
  return 0;
}

#include <thread>

// Resource Acquisition Is Initialization for joining threads

class thread_guard {
  std:: thread& t;
 public:
  explicit thread_guard(std::thread& t_)
      : t(t_) {
  }
  ~thread_guard() {
    if (t.joinable()) {
      t.join();
    }
  }
  thread_guard(thread_guard const&)=delete;
  thread_guard& operator=(thread_guard const&)=delete;
};

// Don't need to worry about handling exceptions in original
// thread.
// The copy constructor and copy-assignment operator are marked
// =delete to ensure that they're not automatically provided
// by the compiler.  Copying or assigning such an object because
// it would be easy for the new object to escape the scope of
// the original object and then any local variables passed
// by ref to the new thread would be gone, but ref'd in the copied
// thread... in other words, doing really unsafe and complicated stuff...



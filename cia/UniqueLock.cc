class some_big_object;
void swap(some_big_object& lhs, some_big_object& rhs);

class X {
 private:
  some_big_object some_detail;
  std::mutex m;
 public:
  X(some_big_object const& sd) : some_detail(sd) {}

  friend void swap(X& lhs, X& rhs) {
    if (&lhs == &rhs) {
      return;
    }
    // std::defer_lock leaves mutexes unlocked
    std::unique_lock<std::mutex> lock_a(lhs.m, std::defer_lock);
    std::unique_lock<std::mutex> lock_b(rhs.m, std::defer_lock);
    std::lock(lock_a, lock_b);
    swap(lhs.some_detail, rhs.some_detail);
  }
};

// std::unique_lock has a owns_lock() member function. This is
// additional overhead compared to std::lock_guard.

// std::lock_guard is preferred unless:
//   1) need deferred locking, or
//   2) need to transfer ownership of a mutex (e.g. escape scope)


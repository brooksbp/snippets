#include <queue>
#include <mutex>
#include <memory>
#include <condition_variable>

template<typename T>
class threadsafe_queue {
 private:
  // mutex must be mutable so it can be locked in empty() and in copy ctor
  mutable std::mutex mut;
  std::queue<T> data_queue;
  std::condition_variable data_cond;
 public:
  threadsafe_queue() {}
  threadsafe_queue(threadsafe_queue const& other) {
    std::lock_guard<std::mutex> lk(other.mut);
    data_queue = other.data_queue;
  }
  void push(T new_value) {
    std::lock_guard<std::mutex> lk(mut);
    data_queue.push(new_value);
    // .notify_one() just notifies one thread in case multiple are waiting
    data_cond.notify_one();
  }
  void wait_and_pop(T& value) {
    std::unique_lock<std::mutex> lk(mut);
    data_cond.wait(lk, [this]{ return !data_queue.empty(); });
    value = data_queue.front();
    data_queue.pop();
  }
  std::shared_ptr<T> wait_and_pop() {
    std::unique_lock<std::mutex> lk(mut);
    data_cond.wait(lk, [this]{ return !data_queue.empty(); });
    std::shared_ptr<T> res(std::make_shared<T>(data_queue.front()));
    data_queue.pop();
    return res;
  }
  bool try_pop(T& value) {
    std::lock_guard<std::mutex> lk(mut);
    if (data_queue.empty()) {
      return false;
    }
    value = data_queue.front();
    data_queue.pop();
    return true;
  }
  std::shared_ptr<T> try_pop() {
    std::lock_guard<std::mutex> lk(mut);
    if (data_queue.empty()) {
      return std::shared_ptr<T>();
    }
    std::shared_ptr<T> res(std::make_shared<T>(data_queue.front()));
    data_queue.pop();
    return res;
  }
  bool empty() const {
    std::lock_guard<std::mutex> lk(mut);
    return data_queue.empty();
  }
};

threadsafe_queue<data_chunk> data_queue;

void data_preparation_thread() {
  while (more_data_to_prepare()) {
    data_chunk const data = prepare_data();
    data_queue.push(data);
  }
}

void data_processing_thread() {
  while (true) {
    data_chunk data;
    data_queue.wait_and_pop(data);
    process(data);
    if (is_last_chunk(data)) {
      break;
    }
  }
}

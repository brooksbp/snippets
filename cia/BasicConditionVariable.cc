#include <condition_variable>

std::mutex mut;
std::queue<data_chunk> data_queue;
std::condition_variable data_cond;

void data_preparation_thread() {
  while (more_data_to_prepare()) {
    data_chunk const data = prepare_data();
    std::lock_guard<std::mutex> lk(mut);
    data_queue.push(data);
    data_cond.notify_one();
  }
}

void data_processing_thread() {
  while (true) {
    std::unique_lock<std::mutex> lk(mut);

    data_cond.wait(lk, []{ return !data_queue.empty(); });

    data_chunk data = data_queue.front();
    data_queue.pop();
    lk.unlock();
    process(data);

    if (is_last_chunk(data)) {
      break;
    }
  }
}

// The implementation of wait() checks the condition (calls the
// lambda).  If false, wait() unlocks the mutex and puts the thread in
// a blocked or waiting state. When .notify_one() called thread wakes
// and reacquires th elock, and checks the condition again, returning
// from the call to wait() with the mutex still locked if the
// condition has been satisfied.  Must use unique_lock for ability
// to lock/unlock/lock/unlock...

// Thread could have spurious wake where awake due to scheduler? and
// not be notified.  Beware of side-effects in the condiiton function.


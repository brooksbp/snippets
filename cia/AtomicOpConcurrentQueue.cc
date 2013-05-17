#include <atomic>
#include <thread>
#include <vector>

std::vector<int> queue_data;
std::atomic<int> count;

void populate_queue() {  
  queue_data.clear();
  for (int i = 0; i < number_of_items; i++) {
    queue_data.push_back(i);
  }
  count.store(number_of_items, std::memory_order_release);
}

void consume_queue_items() {
  while (true) {
    int item_index;
    if ((item_index = count.fetch_sub(1, std::memory_order_acquire)) <= 0) {
      // wait_for_more_items();
      continue;
    }
    // process(queue_data[item_index - 1]);
  }
}

int main(int argc, char *argv[]) {
  std::thread a(populate_queue);
  std::thread b(consume_queue_items);
  std::thread c(consume_queue_items);
  a.join();
  b.join();
  c.join();
  return 0;
}

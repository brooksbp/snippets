#include <atomic>
#include <thread>
#include <assert.h>

std::atomic<bool> x, y;
std::atomic<int> z;

void write_x_then_y() {
  x.store(true, std::memory_order_relaxed);
  y.store(true, std::memory_order_release);
}
void read_y_then_x() {
  while (!y.load(std::memory_order_acquire)) { }
  if (x.load(std::memory_order_relaxed)) {
    ++z;
  }
}

// variable y release sync-with acquire. x has an ordering
// with y, and since y is sync'd, so is x.

// in the book, the example before this one does a poor
// job of explaining why the assert can fail.  if you look
// at fig 5.6 you can see that the the release-acquire
// syncs, but there is no ordering since the store does
// not happen in the first thread that does the release..
// which would then impose an ordering... imo this example
// should have come before the one here.

int main(int argc, char *argv[]) {
  x = false;
  y = false;
  z = 0;
  std::thread a(write_x_then_y);
  std::thread b(read_y_then_x);
  a.join();
  b.join();
  assert(z.load() != 0);
  return 0;
}

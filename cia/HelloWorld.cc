// clang++ HelloWorld.cc -std=c++11 -pthread

#include <iostream>
#include <thread>

void hello() {
  std::cout << "Hello Concurrent World!" << std::endl;
}

int main(int argc, char *argv[]) {
  std::thread t(hello);
  t.join();
  return 0;
}

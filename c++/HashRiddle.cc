#include <iostream>
#include <string>
#include <stdint.h>

uint64_t checksum(std::string& s) {
  uint64_t result = 0;
  for (auto c : s) {
    result <<= 7;
    result ^= c;
  }
  return result;
}

int main() {
  std::string msg = "The answer to this question is: 123454321";
  std::cout << checksum(msg) << std::endl;
  // Expected Output: 3559195706944969009
  return 0;
}

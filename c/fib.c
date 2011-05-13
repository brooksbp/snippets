#include <stdio.h>

int fib(int n);

int main() {
  printf("fib(5)=%d", fib(5));
  return 0;
}

int fib(int n) {
  if (n == 1) return 0;
  if (n == 2) return 1;
  return fib(n-1) + fib(n-2);
}

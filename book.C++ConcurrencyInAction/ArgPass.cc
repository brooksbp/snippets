#include <thread>
#include <string>

void f(int i, std::string const& s);
void oops(int some_param) {
  char buffer[1024];
  sprintf(buffer, "%i", some_param);
  std::thread t(f, 3, buffer);
  t.detach();
}
// Significant change that oops will exit before buffer has been
// converted to std::string on the new thread.  So cast it before:

{
  std::thread t(f, 3, std::string(buffer));
}

// std::thread constructor is oblivious to types of args, so if
// you pass a value and the prototype takes a reference, the
// constructor will copy the value, and will call the fn with a
// reference of the internal copy instead of the value in the
// original thread.  Use std::bind ..?

void f(int a, int& b);

{
  int b = 4;
  std::thread t(f, 3, b);              // wrong
  std::thread t(f, 3, std::ref(b));
}


#include <mutex>
#include <memory>

std::shared_ptr<some_resources> resource_ptr;
std::mutex resource_mutex;

void foo() {
  std::unique_lock<std::mutex> lk(resource_mutex);
  if (!resource_ptr) {
    resource_ptr.reset(new some_resource);
  }
  lk.unlock();
  resource_ptr->do_something();
}

// Classic thread-safe lazy initialization example.

// Onced init, the mutex is pure overhead for lifetime of program.

// Infamous Double-Checked Locking pattern:
// The pointer is first read without acquiring the lock, and the lock
// is acquired only if the pointer is NULL. The pointer is then checked
// again once the lock has been acquired in case another thread has
// done the initialzatoin between the first check and this thread
// acquiring the lock.

void undefined_behavior_with_double_checked_locking() {
  if (!reousrce_ptr) {
    std::lock_guard<std::mutex> lk(resource_mutex);
    if (!resource_ptr) {
      resource_ptr.reset(new some_resource);
    }
  }
  resource_ptr->do_something();
}

// The first resource_ptr check does not guarentee that the
// new some_resource has been fully allocated and ready to
// call do_something() against!  'data race' and has
// undefined behavior

// std::call_once should generally have lower overhead that a normal
// mutex.

std::shared_ptr<some_resource> resource_ptr;
std::once_flag resource_flag;

void init_resource() {
  resource_ptr.reset(new some_resource);
}
void foo() {
  std::call_once(resource_flag, init_resource);
  resource_ptr->do_something();
}

// Thread-safe lazy initialization of a class member

class X {
 private:
  connection_info connection_details;
  connection_handle connection;
  std::once_flag connection_init_flag;

  void open_connection() {
    connection = connection_manager.open(connection_details);
  }
 public:
  X(connection_info const& connection_details_) :
      connection_details(connection_details_) {
  }
  void send_data(data_packet const& data) {
    std::call_once(connection_init_flag, &X::open_connection, this);
    connection.send_data(data);
  }
  data_packet receive_data() {
    std::call_once(connection_init_flag, &X::open_connection, this);
    return connection.receive_data();
  }
};

// C++11 guarentees thread-safe (only one thread will do it)
// initialization of static variables.

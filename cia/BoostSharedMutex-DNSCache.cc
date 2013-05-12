// C++11 proposal, but was not accepted.  Will use Boost example:

#include <map>
#include <string>
#include <mutex>
#include <boost/thread/shared_mutex.hpp>

class dns_entry;

class dns_cache {
  std::map<std::string, dns_entry> entries;
  mutable boost::shared_mutex entry_mutex;
 public:
  dns_entry find_entry(std::string const& domain) const {
    // shared, read-only access
    boost::shared_lock<boost::shared_mutex> lk(entry_mutex);
    std::map<std::string, dns_entry>::const_iterator const it =
        entries.find(domain);
    return (it == entries.end()) ? dns_entry() : it->second;
  }
  void update_or_add_entry(std::string const& domain,
                           dns_entry const& dns_details) {
    // exclusive access, blocks against read-only access as well
    std::lock_guard<boost::shared_mutex> lk(entry_mutex);
    entries[domain] = dns_details;
  }
};

// Cool.. but what about fairness...

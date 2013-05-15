std::shared_ptr<my_data> p;

void process_global_data() {
  std::shared_ptr<my_data> local = std::atomic_load(&p);
  process_data(local);
}

void alloc_global_data() {
  std::shared_ptr<my_data> local(new my_data);
  std::atomic_store(&p, local);
}

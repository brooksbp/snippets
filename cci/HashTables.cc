// Hash table (hash map) used to implement an associative array,
// a structure that maps keys to values. A hash table uses a hash
// function to compute an index into an array of buckets from which
// the correct value can be found.

// hash = hashfunc(key)
// index = hash % array_size

// if array size is a power of 2, the remainder operation can be
// reduced to just masking the hash/index.

// separate chaining (open hashing, closed addressing) - list of
// entries with the same index (for collisions).

// open addressing (closed hashing) - for collisions, proceed with
// some probe sequence until an unoccupied slot is found.
// linear probing (slot += 1)
// quadratic probing (slot += quadratic polynomial value)
// double hashing (slot += hash the index with another hash function)

// Dynamic resizing to keep load factor < 0.75 as in Java's HashMap.
// Resizing is accompanied by a full or incremental table rehash where
// existing items are mapped to new bucket locations.
// Shrink when load too low.
// Resize by copying - allocate new table and add elements, then delete
// old table.
// Incremental resizing (real-time systems) - alloc new table,
// lookup or delete check both tables, insert in new, delete from old.

// implement it as a binary search tree

template <class K, class V>
class HashTbl {
  explicit HashTbl(const key_equal& eq = key_equal(),
                   const hasher& hf = hasher())
      : eq, hf {
    // clear?
  }
};

int main(int argc, char *argv[]) {

  HashTbl<int, int> tbl;
  
  return 0;
}

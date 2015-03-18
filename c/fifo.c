// credits to ryg

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define SIZE 16

typedef uint32_t ElemType;

struct FIFO {
  // Usually stored in some block of memory somewhere.
  ElemType Elem[SIZE];

  // Usually memory-mapped registers. In software, define one before
  // the array and one after, and make sure they're aligned to cache
  // line size (avoid false sharing for parallel read/write).
  size_t ReadPos;
  size_t WritePos;
};


// ---------------------------------------------------------------------
// Model 1: Just array indices (or pointers)

// Normally what you have when talking to hardware.

void Write(struct FIFO* f, ElemType e) {
  f->Elem[f->WritePos] = e;
  f->WritePos = (f->WritePos + 1) % SIZE;
}

void Read(struct FIFO* f, ElemType *e) {
  *e = f->Elem[f->ReadPos];
  f->ReadPos = (f->ReadPos + 1) % SIZE;
}

// If WritePos == ReadPos, the queue is empty.

// Could add field to FIFO to keep track of elem count...

// Or, stop adding elements to the queue when it contains SIZE-1 elems.

// TODO

// ---------------------------------------------------------------------
// Model 2: Virtual stream

// Giving the 'distance travelled' from the start rather than the actual
// position in the buffer. No wrapping.. If buffer has wrapped twice,
// your index is 2*SIZE not 0.

void Write(struct FIFO* f, ElemType e) {
  f->Elem[f->WritePos % SIZE] = e;
  f->WritePos++;
}

void Read(struct FIFO* f, ElemType *e) {
  *e = f->Elem[f->ReadPos % SIZE];
  f->ReadPos++;
}

// Pick SIZE to be a power of 2 for the mod to be efficient.

// Now, number of items is much easier to compute: WritePos - ReadPos.

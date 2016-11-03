// $ gcc -S -O0 atomics_builtin.c -o atomics_builtin.x86_64
// $ aarch64-linux-gnu-gcc -S -O0 atomics_builtin.c -o atomics_builtin.x86_64

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#define fence(ord) \
	void fence_ ## ord (void) { \
		__atomic_thread_fence(__ATOMIC_ ## ord ); \
	}

fence(RELAXED)
fence(CONSUME)
fence(ACQUIRE)
fence(RELEASE)
fence(ACQ_REL)
fence(SEQ_CST)

static uint64_t u64;

#define load(ord) \
	void load_ ## ord (void) { \
		uint64_t u64_; \
		u64_ = __atomic_load_n(&u64_, __ATOMIC_ ## ord ); \
	}

load(RELAXED)
load(CONSUME)
load(ACQUIRE)
//load(RELEASE)
//load(ACQ_REL)
load(SEQ_CST)

#define store(ord) \
	void store_ ## ord (void) { \
		__atomic_store_n(&u64, 64, __ATOMIC_ ## ord ); \
	}

store(RELAXED)
//store(CONSUME)
//store(ACQUIRE)
store(RELEASE)
//store(ACQ_REL)
store(SEQ_CST)

#define add_fetch(ord) \
	void add_fetch_ ## ord (void) { \
		uint64_t u64_; \
		u64_ = __atomic_add_fetch(&u64, 1, __ATOMIC_ ## ord ); \
	}

add_fetch(RELAXED)
add_fetch(CONSUME)
add_fetch(ACQUIRE)
add_fetch(RELEASE)
add_fetch(ACQ_REL)
add_fetch(SEQ_CST)

#define cmp_xchg(ord) \
	void cmp_xchg_ ## ord (void) { \
		uint64_t u64_ = 0; \
		u64_ = __atomic_compare_exchange_n(&u64, &u64_, 1, \
						   false, __ATOMIC_ ## ord, __ATOMIC_RELAXED ); \
	}

cmp_xchg(RELAXED)
cmp_xchg(CONSUME)
cmp_xchg(ACQUIRE)
cmp_xchg(RELEASE)
cmp_xchg(ACQ_REL)
cmp_xchg(SEQ_CST)


int main(void)
{
	fence_RELAXED();
	fence_CONSUME();
	fence_ACQUIRE();
	fence_RELEASE();
	fence_ACQ_REL();
	fence_SEQ_CST();
	load_RELAXED();
	load_CONSUME();
	load_ACQUIRE();
	//load_RELEASE();
	//load_ACQ_REL();
	load_SEQ_CST();
	store_RELAXED();
	//store_CONSUME();
	//store_ACQUIRE();
	store_RELEASE();
	//store_ACQ_REL();
	store_SEQ_CST();

	add_fetch_RELAXED();
	add_fetch_CONSUME();
	add_fetch_ACQUIRE();
	add_fetch_RELEASE();
	add_fetch_ACQ_REL();
	add_fetch_SEQ_CST();

	cmp_xchg_RELAXED();
	cmp_xchg_CONSUME();
	cmp_xchg_ACQUIRE();
	cmp_xchg_RELEASE();
	cmp_xchg_ACQ_REL();
	cmp_xchg_SEQ_CST();
return 0;
}

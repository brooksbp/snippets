#include <stdio.h>

void GetCpuIDAndInfo(unsigned value, unsigned *rEAX,
                     unsigned *rEBX, unsigned *rECX,
                     unsigned *rEDX) {
#if defined(__x86_64__) || defined(_M_AMD64) || defined (_M_X64)
#if defined(__GNUC__)
  // gcc doesn't know cpuid would clobber ebx/rbx. Preseve it manually.
  asm ("movq\t%%rbx, %%rsi\n\t"
       "cpuid\n\t"
       "xchgq\t%%rbx, %%rsi\n\t"
       : "=a" (*rEAX),
         "=S" (*rEBX),
         "=c" (*rECX),
         "=d" (*rEDX)
       :  "a" (value));
#elif defined(_MSC_VER)
  int registers[4];
  __cpuid(registers, value);
  *rEAX = registers[0];
  *rEBX = registers[1];
  *rECX = registers[2];
  *rEDX = registers[3];
#endif
#endif
}

int main(void) {
  unsigned EAX = 0, EBX = 0, ECX = 0, EDX = 0;
  GetCpuIDAndInfo(0x1, &EAX, &EBX, &ECX, &EDX);
  if ((ECX >> 13) & 0x1) {
    printf("cmpxchg16b\n");
  }
  return 0;
}

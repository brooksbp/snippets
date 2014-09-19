#include <stdio.h>

#include <cpuid.h>
#include <x86intrin.h>
#include <popcntintrin.h>

long long work() {
  long long accum = 0;
  int i;

  for (i = 0; i < 0xff; i++)
    accum += __builtin_popcount(i);
  
  return accum;
}

long long ReadTSC() {
  int a, b, c, d;
  volatile int dont_skip;
  long long clock;

  // serialize; flush pipeline before cpuid
  __cpuid(0, a, b, c, d);
  // prevent optimizing away cpuid
  dont_skip = a;

  clock = __rdtsc();
  return clock;
}

int main() {
  const int num_tests = 10;
  int i;
  long long t;
  long long times[num_tests];

  for (i = 0; i < num_tests; i++) {
    t = ReadTSC();

    work();

    times[i] = ReadTSC() - t;
  }

  for (i = 0; i < num_tests; i++)
    printf("%2i %10lli\n", i, times[i]);
  
  return 0;
}


// gcc -o popcnt popcnt.c
//
//  $ ./popcnt 
//   0       7089
//   1       6681
//   2       6630
//   3       6537
//   4       6596
//   5       6536
//   6       6596
//   7       6537
//   8       6596
//   9       6536
//
// objdump -d popcnt
//
//  0000000000400506 <work>:
//    400506:	55                   	push   %rbp
//    400507:	48 89 e5             	mov    %rsp,%rbp
//    40050a:	48 83 ec 10          	sub    $0x10,%rsp
//    40050e:	48 c7 45 f8 00 00 00 	movq   $0x0,-0x8(%rbp)
//    400515:	00 
//    400516:	c7 45 f4 00 00 00 00 	movl   $0x0,-0xc(%rbp)
//    40051d:	eb 17                	jmp    400536 <work+0x30>
//    40051f:	8b 45 f4             	mov    -0xc(%rbp),%eax
//    400522:	89 c0                	mov    %eax,%eax
//    400524:	48 89 c7             	mov    %rax,%rdi
//    400527:	e8 64 01 00 00       	callq  400690 <__popcountdi2>
//    40052c:	48 98                	cltq   
//    40052e:	48 01 45 f8          	add    %rax,-0x8(%rbp)
//    400532:	83 45 f4 01          	addl   $0x1,-0xc(%rbp)
//    400536:	81 7d f4 fe 00 00 00 	cmpl   $0xfe,-0xc(%rbp)
//    40053d:	7e e0                	jle    40051f <work+0x19>
//    40053f:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
//    400543:	c9                   	leaveq 
//    400544:	c3                   	retq   
//  
//  0000000000400690 <__popcountdi2>:
//    400690:	48 89 fa             	mov    %rdi,%rdx
//    400693:	48 b8 55 55 55 55 55 	movabs $0x5555555555555555,%rax
//    40069a:	55 55 55 
//    40069d:	48 d1 ea             	shr    %rdx
//    4006a0:	48 21 d0             	and    %rdx,%rax
//    4006a3:	48 ba 33 33 33 33 33 	movabs $0x3333333333333333,%rdx
//    4006aa:	33 33 33 
//    4006ad:	48 29 c7             	sub    %rax,%rdi
//    4006b0:	48 89 f8             	mov    %rdi,%rax
//    4006b3:	48 c1 ef 02          	shr    $0x2,%rdi
//    4006b7:	48 21 d0             	and    %rdx,%rax
//    4006ba:	48 21 d7             	and    %rdx,%rdi
//    4006bd:	48 ba 01 01 01 01 01 	movabs $0x101010101010101,%rdx
//    4006c4:	01 01 01 
//    4006c7:	48 01 c7             	add    %rax,%rdi
//    4006ca:	48 89 f8             	mov    %rdi,%rax
//    4006cd:	48 c1 e8 04          	shr    $0x4,%rax
//    4006d1:	48 01 c7             	add    %rax,%rdi
//    4006d4:	48 b8 0f 0f 0f 0f 0f 	movabs $0xf0f0f0f0f0f0f0f,%rax
//    4006db:	0f 0f 0f 
//    4006de:	48 21 c7             	and    %rax,%rdi
//    4006e1:	48 89 f8             	mov    %rdi,%rax
//    4006e4:	48 0f af c2          	imul   %rdx,%rax
//    4006e8:	48 c1 e8 38          	shr    $0x38,%rax
//    4006ec:	c3                   	retq   
//    4006ed:	0f 1f 00             	nopl   (%rax)


// gcc -mpopcnt -o popcnt popcnt.c
//
//  $ ./popcnt 
//   0       5704
//   1       5551
//   2       4666
//   3       4837
//   4       4735
//   5       5032
//   6       4734
//   7       4632
//   8       4726
//   9       4582
//
// objdump -d popcnt
//
//  0000000000400506 <work>:
//    400506:	55                   	push   %rbp
//    400507:	48 89 e5             	mov    %rsp,%rbp
//    40050a:	48 c7 45 f8 00 00 00 	movq   $0x0,-0x8(%rbp)
//    400511:	00 
//    400512:	c7 45 f4 00 00 00 00 	movl   $0x0,-0xc(%rbp)
//    400519:	eb 11                	jmp    40052c <work+0x26>
//    40051b:	8b 45 f4             	mov    -0xc(%rbp),%eax
//    40051e:	f3 0f b8 c0          	popcnt %eax,%eax
//    400522:	48 98                	cltq   
//    400524:	48 01 45 f8          	add    %rax,-0x8(%rbp)
//    400528:	83 45 f4 01          	addl   $0x1,-0xc(%rbp)
//    40052c:	81 7d f4 fe 00 00 00 	cmpl   $0xfe,-0xc(%rbp)
//    400533:	7e e6                	jle    40051b <work+0x15>
//    400535:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
//    400539:	5d                   	pop    %rbp
//    40053a:	c3                   	retq   

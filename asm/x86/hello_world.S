// gcc -c hello_world.S
// ld hello_world.o
// or, gcc -nostdlib hello_world.S

        .global _start

        .text
_start:
        // write(1, message, 13)
        mov     $1, %rax        // syscall 1: write
        mov     $1, %rdi        // fd 1: stdout
        mov     $message, %rsi  // load the address
        mov     $13, %rdx       // len(message)
        syscall

        // exit(0)
        mov     $60, %rax       // syscall 60: exit
        xor     %rdi, %rdi      // 0
        syscall
message:
        .ascii  "Hello, World\n"

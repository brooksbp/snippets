.section .rodata

b64table:
        .byte 'A,'B,'C,'D,'E,'F,'G,'H
        .byte 'I,'J,'K,'L,'M,'N,'O,'P
        .byte 'Q,'R,'S,'T,'U,'V,'W,'X
        .byte 'Y,'Z,'a,'b,'c,'d,'e,'f
        .byte 'g,'h,'i,'j,'k,'l,'m,'n
        .byte 'o,'p,'q,'r,'s,'t,'u,'v
        .byte 'w,'x,'y,'z,'0,'1,'2,'3
        .byte '4,'5,'6,'7,'8,'9,'+,'/

formatStr:
        .ascii "\nPlain data: %s\nEncoded data: %s\n\n\0"

.section .bss

        .comm encodedData, 1024

.section .data

plainData:
        .ascii "Hello World!\0"
plainDataLen:
        .int 12

        
.section .text

.global main
main:
        movl $0, %ecx           # counter
        movl plainDataLen, %edx
        movl $plainData, %esi
        movl $encodedData, %edi
        movl $b64table, %ebp

b64_encode_3b:
        movb (%esi, %ecx, 1), %al # byte 1 of 3
        incl %ecx
        shl $16, %eax
        movb (%esi, %ecx, 1), %ah # byte 2 of 3
        incl %ecx
        movb (%esi, %ecx, 1), %al # byte 3 of 3
        incl %ecx

        movl %eax, %ebx                 # save a copy of %eax

        ## look up base-64 char 1
        shr $18, %eax                   # shift top 6-bits to bottom
        andl $0x3f, %eax                # mask them off
        movb (%ebp, %eax, 1), %al       # look up char in b64table
        movb %al, (%edi)                # write char to encodeString
        incl %edi
        movl %ebx, %eax

        ## look up base-64 char 2
        shr $12, %eax
        andl $0x3f, %eax
        movb (%ebp, %eax, 1), %al
        movb %al, (%edi)
        incl %edi
        movl %ebx, %eax

        ## look up base-64 char 3
        shr $6, %eax
        andl $0x3f, %eax
        movb (%ebp, %eax, 1), %al
        movb %al, (%edi)
        incl %edi
        movl %ebx, %eax

        ## look up base-64 char 4
        andl $0x3f, %eax
        movb (%ebp, %eax, 1), %al
        movb %al, (%edi)
        incl %edi

        cmpl %edx, %ecx
        jl b64_encode_3b

        ## null-terminate encoded string
        movb $0, %al
        movb %al, (%edi)

        ## print encoded string
        movq $formatStr, %rdi
        movq $plainData, %rsi
        movq $encodedData, %rdx
        call printf

        ret

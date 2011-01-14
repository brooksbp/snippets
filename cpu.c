/* cpu.c */

#include <stdio.h>
#include <stdint.h>


/*
 * Memory layout
 *
 *      ---------------------
 *    0 | 4 bytes / 32 bits |
 *      ---------------------
 *    4 |                   |
 *      ---------------------
 *    8 |                   |
 *      ---------------------
 *              ...
 * 0xff |                   |
 *      ---------------------
 */
#define MEM_MAX_ADDR 0xff
#define MEM_MAX_SIZE (MEM_MAX_ADDR << 1)    /* 256 bytes */

uint8_t mem[MEM_MAX_SIZE];


/* Registers */
#define zero r[0]
#define at   r[1]
#define v0   r[2]
#define v1   r[3]
#define a0   r[4]
#define a1   r[5]
#define a2   r[6]
#define a3   r[7]
#define t0   r[8]
#define t1   r[9]
#define t2   r[10]
#define t3   r[11]
#define t4   r[12]
#define t5   r[13]
#define t6   r[14]
#define t7   r[15]
#define s0   r[16]
#define s1   r[17]
#define s2   r[18]
#define s3   r[19]
#define s4   r[20]
#define s5   r[21]
#define s6   r[22]
#define s7   r[23]
#define a3   r[24]
#define a3   r[25]
#define a3   r[26]
#define a3   r[27]
#define a3   r[28]
#define a3   r[29]
#define a3   r[30]
#define a3   r[31]



/*
 * Instruction Format
 *
 * b31-b26 op
 * b25-b21 rs
 * b20-b16 rt
 * b15-b0  const|addr
 *
 */


int main() {

    /* Initialize registers */
    uint32_t r[32];


    printf("hello, world!\n");


    /* Always start executing program at 0xff */
    

    return 0;
}

// Simple example for Microchip 16F690 Processor
//
// sdcc -mpic14 -p16f690 blinking-led.c
// pk2cmd -PPIC16f690 -Fblinking-led.hex -M -A5.0 -T
// pk2cmd -PPIC16f690 -E
// pk2cmd -PPIC16f690 -C

#define __16f690
#include <pic/pic16f690.h>

unsigned int at 0x2007 __CONFIG =
    _INTRC_OSC_NOCLKOUT & _WDT_OFF &
    _PWRTE_OFF & _MCLRE_OFF & _CP_OFF &
    _BOR_OFF & _IESO_OFF & _FCMEN_OFF;

void wait(void) {
  unsigned long delay = 0;
  while (delay < 100000) {
    delay++;
  }
}

unsigned int led = 0;

void main(void) {

  // Internal oscillator is used for system clock
  SCS = 1;
  // 8 MHz
  IRCF2 = 1; IRCF1 = 1; IRCF0 = 1;

  ANSEL = 0x00;
  ANSELH = 0x00;

  TRISA = 0x00;
  TRISB = 0x00;
  TRISC = 0x00;

  PORTA = 0x00;
  PORTB = 0x00;
  PORTC = 0x00;

  led = 0;
  
  for (;;) {
    PORTC = led;
    led += 1;
    wait();
  }
}

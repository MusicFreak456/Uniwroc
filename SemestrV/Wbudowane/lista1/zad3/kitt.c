#include <avr/io.h>
#include <util/delay.h>
#include <inttypes.h>

#define BAR_DDR DDRD
#define DISPLAY_PORT PORTD

#define BAR_SIZE 8

int main() {
  uint8_t state = 1;
  UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
  BAR_DDR = 0xFF;
  DISPLAY_PORT = state;
  while (1) {
    for(int i=0; i < BAR_SIZE - 1; i++){
      _delay_ms(80);
      state <<= 1;
      DISPLAY_PORT = state;
    }
    for (int i = 0; i < BAR_SIZE - 1; i++)
    {
      _delay_ms(80);
      state >>= 1;
      DISPLAY_PORT = state;
    }
  }
}

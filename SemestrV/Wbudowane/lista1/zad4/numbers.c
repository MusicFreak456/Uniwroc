#include <avr/io.h>
#include <util/delay.h>

#define A _BV(PD0)
#define B _BV(PD1)
#define C _BV(PD2)
#define D _BV(PD3)
#define E _BV(PD4)
#define F _BV(PD5)
#define G _BV(PD6)
#define DC _BV(PD7)

#define ZERO ~(A | B | C | D | E | F)
#define ONE ~(B | C)
#define TWO ~(A | B | G | E | D)
#define THREE ~(A| B| C| D| G)
#define FOUR ~(F | G | B | C)
#define FIVE ~(A | F | G | C | D)
#define SIX ~(A | F | C | D | E | G)
#define SEVEN ~(A | B | C)
#define EIGHT ~(A | B | C | D | E | F | G)
#define NINE ~(A | B | C | D | F | G)

#define DISPLAY_DDR DDRD
#define DISPLAY_PORT PORTD

#define QUANTITY 10

const int NUMBERS[QUANTITY] = {
  ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
};

int main() {
  UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
  DISPLAY_DDR = 0xFF;
  while (1) {
    for(int i=0; i < QUANTITY; i++){
      DISPLAY_PORT = NUMBERS[i];
      _delay_ms(1000);
    }
  }
}

#include <avr/io.h>
#include <inttypes.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

#define A _BV(0)
#define B _BV(1)
#define C _BV(2)
#define D _BV(3)
#define E _BV(4)
#define F _BV(5)
#define G _BV(6)
#define DC _BV(7)

#define ZERO  (A | B | C | D | E | F)
#define ONE   (B | C)
#define TWO   (A | B | G | E | D)
#define THREE (A | B | C | D | G)
#define FOUR  (F | G | B | C)
#define FIVE  (A | F | G | C | D)
#define SIX   (A | F | C | D | E | G)
#define SEVEN (A | B | C)
#define EIGHT (A | B | C | D | E | F | G)
#define NINE  (A | B | C | D | F | G)

#define LOAD _BV(PB1)
#define LED_DISABLE _BV(PB2)
#define CTRL_DDR DDRB
#define CTRL_PORT PORTB

void spi_init()
{
  // ustaw piny MOSI, SCK i ~SS jako wyjścia
  DDRB |= _BV(DDB3) | _BV(DDB5) | _BV(DDB2);
  // włącz SPI w trybie master z zegarem 250 kHz
  SPCR = _BV(SPE) | _BV(MSTR) | _BV(SPR1);
  // włącz przerwania SPI
  SPCR |= _BV(SPIE);
}

void timer1_init() {
  // ustaw tryb licznika
  // COM1A = 10   -- non-inverting mode
  // WGM1  = 1110 -- fast PWM top=ICR1
  // CS1   = 101  -- prescaler 1024
  // ICR1  = 15624
  // częstotliwość 16e6/(1024*(1+15624)) = 1 Hz
  ICR1 = 15624;
  TCCR1A = _BV(WGM11);
  TCCR1B = _BV(WGM12) | _BV(WGM13) | _BV(CS10) | _BV(CS12);
  TIMSK1 = _BV(TOIE1);
}


#define QUANTITY 10

ISR(SPI_STC_vect) {
  CTRL_PORT |= LOAD;
  CTRL_PORT &= ~LOAD;
}

volatile int index;
const int NUMBERS[QUANTITY] = {
  ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
};

ISR(TIMER1_OVF_vect) {
  SPDR = NUMBERS[index];
  index = (index == QUANTITY - 1) ? 0 : index + 1;
}

int main()
{
  CTRL_DDR |= LOAD | LED_DISABLE;
  CTRL_PORT &= ~LOAD;
  CTRL_PORT &= ~LED_DISABLE;

  spi_init();
  timer1_init();

  sei();

  set_sleep_mode(SLEEP_MODE_IDLE);

  SPDR = NUMBERS[0];
  index++;

  while(1) {
    sleep_mode();
  }
}

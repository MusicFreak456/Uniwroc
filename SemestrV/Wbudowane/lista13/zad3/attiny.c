#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>

#define LED PB2
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB1
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define BUFFER_SIZE 63

typedef int8_t cyclic_buffer[BUFFER_SIZE];
cyclic_buffer buffer = {0};
int8_t index = 0;

void next() {
  index = (index == BUFFER_SIZE - 1) ? 0 : (index + 1);
}

void write(int8_t val) {
  buffer[index] = val;
}

int8_t read() {
  return buffer[index];
}

void timer0_init()
{
  // CTC mode
  // Preskaler: 256
  // Top: 60
  // Częstotliwość: około 63 Hz
  TCCR0A = _BV(WGM01);
  OCR0A = 60;
  TCCR0B =  _BV(CS02);
  TIMSK0 |= _BV(OCIE0A);
}

ISR(TIM0_COMPA_vect) {
  if(read()) {
    LED_PORT |= _BV(LED);
  } else {
    LED_PORT &= ~_BV(LED);
  }

  if(BTN_PIN & _BV(BTN)) write(0);
  else write(1);
  next();
}

int main() {
  BTN_PORT = _BV(BTN);
  LED_DDR |= _BV(LED);

  timer0_init();
  set_sleep_mode(SLEEP_MODE_IDLE);
  sei();

  while (1) {
    sleep_mode();
  }
}

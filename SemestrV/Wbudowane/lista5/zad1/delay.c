#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>

#define LED PD3
#define LED_DDR DDRD
#define LED_PORT PORTD

#define BTN PC1
#define BTN_PIN PINC
#define BTN_PORT PORTC

#define BUFFER_SIZE 61

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

void timer2_init()
{
  // Preskaler: 1024
  // Top: 0xFF
  // Częstotliwość: około 61 Hz
  TCCR2B = _BV(CS20) | _BV(CS21) | _BV(CS22);
  TIMSK2 |= _BV(TOIE2);
}

ISR(TIMER2_OVF_vect) {
  LED_PORT = read();
  if(BTN_PIN & _BV(BTN)) write(0);
  else write(_BV(LED));
  next();
}

int main() {
  BTN_PORT |= _BV(BTN);
  LED_DDR |= _BV(LED);
  LED_PORT = 0;

  timer2_init();
  set_sleep_mode(SLEEP_MODE_PWR_SAVE);
  sei();

  while (1) {
    sleep_mode();
  }
}

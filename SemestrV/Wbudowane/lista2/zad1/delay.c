#include <avr/io.h>
#include <util/delay.h>

#define LED PD3
#define LED_DDR DDRD
#define LED_PORT PORTD

#define BTN PC0
#define BTN_PIN PINC
#define BTN_PORT PORTC

#define BUFFER_SIZE 100

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

int main() {
  UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
  BTN_PORT |= _BV(BTN);
  LED_DDR |= _BV(LED) | _BV(PD0) | _BV(PD1);
  LED_PORT = 0;
  while (1) {
    LED_PORT = read();
    if(BTN_PIN & _BV(BTN)) write(0);
    else write(_BV(LED));
    _delay_ms(10);
    next();
  }
}

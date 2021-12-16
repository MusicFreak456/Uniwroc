#include <avr/io.h>
#include <util/delay.h>

#define LED PC0
#define LED_DDR DDRC
#define LED_PORT PORTC

#define BTN PC2
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

#define led_on()  LED_PORT |=  _BV(LED)
#define led_off() LED_PORT &= ~_BV(LED)

int main() {
  BTN_PORT |= _BV(BTN);
  LED_DDR  |= _BV(LED);
  led_off();

  while (1) {
    if(read()) led_on();
    else       led_off();

    if(BTN_PIN & _BV(BTN)) write(0);
    else write(_BV(LED));

    _delay_ms(10);
    next();
  }
}

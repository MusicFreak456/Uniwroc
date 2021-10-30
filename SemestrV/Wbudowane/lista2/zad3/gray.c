#include <avr/io.h>
#include <util/delay.h>

#define DISPLAY_DDR DDRD
#define DISPLAY_PORT PORTD

#define BTN_PREV PC0
#define BTN_RESET PC1
#define BTN_NEXT PC2
#define BTN_PIN PINC
#define BTN_PORT PORTC

#define INITIAL_STATE 0

uint8_t counter_state;
int8_t current_buttons_state;

int8_t get_buttons_state() {
  int state = 0;
  state = !(BTN_PIN & _BV(BTN_PREV ));
  state |= !(BTN_PIN & _BV(BTN_RESET)) << 1;
  state |= !(BTN_PIN & _BV(BTN_NEXT )) << 2;
  return state;
}

int8_t increase_counter() {
  counter_state += 1;
}

int8_t decrease_counter() {
  counter_state -= 1;
}

int8_t reset_counter() {
  counter_state = INITIAL_STATE;
}

uint8_t decimal_to_gray(uint8_t num) {
  return num ^ (num >> 1);
}

// Opcjonalny debounce software'owy
// int8_t debounce(int8_t new_buttons_state) {
//   _delay_ms(10);
//   int8_t control_buttons_state = get_buttons_state();
//   return control_buttons_state == new_buttons_state;
// }

void handle_change(int new_buttons_state) {
  int8_t change = (current_buttons_state ^ new_buttons_state);
  change &= new_buttons_state;
  if(change & 1) decrease_counter();
  if(change & 2) reset_counter();
  if(change & 4) increase_counter();
}

int main() {
  UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
  BTN_PORT |= 0x07;
  DISPLAY_DDR |= 0xFF;
  DISPLAY_PORT = 0;
  int8_t new_buttons_state;
  current_buttons_state = get_buttons_state();
  while (1) {
    new_buttons_state = get_buttons_state();
    if(new_buttons_state != current_buttons_state/* && 
       debounce(new_buttons_state)*/) {
      handle_change(new_buttons_state);
      current_buttons_state = new_buttons_state;
      DISPLAY_PORT = decimal_to_gray(counter_state);
    }
  }
}

#include <avr/io.h>
#include <avr/sleep.h>
#include <inttypes.h>
#include <avr/interrupt.h>

#define LED _BV(PB5)
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

// inicjalizacja UART
void uart_init()
{
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
  // wyczyść rejestr UCSR0A
  UCSR0A = 0;
  // włącz odbiornik i nadajnik
  UCSR0B = _BV(RXEN0) | _BV(TXEN0);
  // włącz przerwania przy odbiorze
  UCSR0B |= _BV(RXCIE0);
  // ustaw format 8n1
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

volatile uint8_t character;

ISR(USART_RX_vect) {
  character = UDR0;
  UCSR0B &= ~_BV(RXCIE0);
  UCSR0B |= _BV(UDRIE0);
}

ISR(USART_UDRE_vect) {
  LED_PORT = ~LED_PORT;
  UDR0 = character;
  UCSR0B &= ~_BV(UDRIE0);
  UCSR0B |= _BV(RXCIE0);
}

int main() {
  LED_DDR |= LED; 
  LED_PORT |= LED;

  set_sleep_mode(SLEEP_MODE_IDLE);

  uart_init();
  sei();

  while (1) {
    sleep_mode();
  }
}
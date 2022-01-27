#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>

/********POŻYCZONE Z WYKŁADU***********************************/

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
  // ustaw format 8n1
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

// transmisja jednego znaku
int uart_transmit(char data, FILE *stream)
{
  // czekaj aż transmiter gotowy
  while(!(UCSR0A & _BV(UDRE0)));
  UDR0 = data;
  return 0;
}

// odczyt jednego znaku
int uart_receive(FILE *stream)
{
  // czekaj aż znak dostępny
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

FILE uart_file;

/**************************************************************/

#define TOP 1388
#define CNT_FREQ 16000000 / 256

void timer1_init()
{
  // WGM1  = 1111 -- fast-PWM, top=OCR1A
  TCCR1A = _BV(WGM11) | _BV(WGM10);
  TCCR1B = _BV(WGM12) | _BV(WGM13);
  OCR1A = TOP;
  // CS1   = 100  -- prescaler 256 -- częstotliowść około 180 Hz
  // ICES1 = input capture na zboczu narastającym
  TCCR1B |= _BV(CS12) | _BV(ICES1);
  // włącz przerwania na input capture
  TIMSK1 = _BV(ICIE1);
}

void timer0_init() {
  // Preskaler: 1024
  // Top: 0xFF
  // Częstotliwość: około 61 Hz
  TCCR0A = _BV(WGM00) | _BV(WGM01) | _BV(COM0A1);
  TCCR0B = _BV(CS02) | _BV(CS00);
  OCR0A = 0xFF / 2;
  DDRD |= _BV(PD6);
}

volatile uint8_t number_of_mesurments;

volatile uint16_t timestamps[2];
volatile uint16_t diff;
volatile uint16_t frequency;

ISR(TIMER1_CAPT_vect) {
  timestamps[number_of_mesurments] = ICR1;
  number_of_mesurments++;
  if(number_of_mesurments == 2){
    uint16_t start = timestamps[0];
    uint16_t end = timestamps[1];

    if(start < end) diff = end - start;
    else diff = (TOP - start) + end;

    frequency = CNT_FREQ / diff;
    number_of_mesurments = 0;
  }
}

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  timer1_init();
  timer0_init();

  set_sleep_mode(SLEEP_MODE_IDLE);

  _delay_ms(10);
  sei();
  while(1) {
    sleep_mode();
    sleep_mode();

    cli();
    printf("%"PRIu16" Hz\r\n", frequency);
    number_of_mesurments = 0;
    sei();
  }
}

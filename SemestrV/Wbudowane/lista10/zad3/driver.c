#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
// #include <stdio.h>

#define DIRECTION_DDR  DDRD
#define DIRECTION_PORT PORTD
#define LEFT  _BV(PD4)
#define RIGHT _BV(PD7)
#define TOP 2048

/********POŻYCZONE Z WYKŁADU***********************************/
// #define BAUD 9600                          // baudrate
// #define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

// // inicjalizacja UART
// void uart_init()
// {
//   // ustaw baudrate
//   UBRR0 = UBRR_VALUE;
//   // włącz odbiornik i nadajnik
//   UCSR0B = _BV(RXEN0) | _BV(TXEN0);
//   // ustaw format 8n1
//   UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
// }

// // transmisja jednego znaku
// int uart_transmit(char data, FILE *stream)
// {
//   // czekaj aż transmiter gotowy
//   while(!(UCSR0A & _BV(UDRE0)));
//   UDR0 = data;
//   return 0;
// }

// // odczyt jednego znaku
// int uart_receive(FILE *stream)
// {
//   // czekaj aż znak dostępny
//   while (!(UCSR0A & _BV(RXC0)));
//   return UDR0;
// }

// FILE uart_file;

/**************************************************************/

void timer1_init() {
  // ustaw tryb licznika
  // COM1A = 11   -- inverting mode
  // WGM1  = 1110 -- fast PWM top=ICR1
  // CS1   = 010  -- prescaler 8
  // ICR1  = 2048
  // częstotliwość 16e6/(8*(1+2048)) = około 976 Hz
  ICR1 = TOP;
  TCCR1A  = _BV(COM1A1) | _BV(COM1A0) | _BV(WGM11);
  TCCR1B  = _BV(WGM12) | _BV(WGM13) | _BV(CS11);
  OCR1A = TOP;
  // ustaw pin OC1A (PB1) jako wyjście
  DDRB |= _BV(PB1);
}

void adc_init() {
  ADMUX   = _BV(REFS0); // referencja Vin, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADIE);  // włącz przerwania ADC
  ADCSRA |= _BV(ADATE); // włącz auto-trigger
  ADCSRB  = _BV(ADTS2); // konwersja przy overflow w liczniku 0
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

void timer0_init() {
  // Preskaler: 1024
  // Top: 0xFF
  // Częstotliwość: około 61 Hz
  TCCR0B = _BV(CS02) | _BV(CS00);
  TIMSK0 |= _BV(TOIE0);
}

void turn_right(uint16_t adc) {
  DIRECTION_PORT &= ~LEFT;
  DIRECTION_PORT |= RIGHT;
  OCR1A = TOP - (4 * adc);
}

void turn_left(uint16_t adc) {
  DIRECTION_PORT &= ~RIGHT;
  DIRECTION_PORT |= LEFT;
  OCR1A = TOP - (4 * adc);
}

#define MAX_READING 1024
uint16_t measured_adc;

ISR(ADC_vect) {
  measured_adc = ADC;
  int16_t shifted_adc = measured_adc - (MAX_READING / 2);
  if(shifted_adc <= 0) {
    turn_right(-shifted_adc);
  } else {
    turn_left(shifted_adc);
  }
}

ISR(TIMER0_OVF_vect) {
}

int main() {
  // uart_init();
  // fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  // stdin = stdout = stderr = &uart_file;
  
  DIRECTION_DDR |= LEFT | RIGHT;
  timer1_init();
  adc_init();
  timer0_init();

  sei();
  set_sleep_mode(SLEEP_MODE_IDLE);
  while(1) {
    sleep_mode();
    // cli();
    // printf("%d\r\n",measured_adc);
    // sei();
  }
}

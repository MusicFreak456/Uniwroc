#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <util/delay.h>

/********POŻYCZONE Z WYKŁADU***********************************/
#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

// inicjalizacja UART
void uart_init()
{
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
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

void timer1_init() {
  // ustaw tryb licznika
  // COM1A = 10   -- up -- clear on match, down -- set on match 
  // WGM1  = 1000 -- PWM, Phase and Frequency Correct, top: ICR1
  // CS1   = 010  -- prescaler 8
  // ICR1  = 2048
  // częstotliwość 16e6/(2*8*1024) = około 976 Hz
  ICR1    = 1024;
  TCCR1A  = _BV(COM1A1);
  TCCR1B  = _BV(WGM13) | _BV(CS11);
  DDRB   |= _BV(PB1);   // ustaw pin OC1A (PB1) jako wyjście
}

void adc_init() {
  ADMUX   = _BV(REFS0) | _BV(MUX0); // referencja Vin, wejście ADC1
  DIDR0   = _BV(ADC1D);             // wyłącz wejście cyfrowe na ADC1
  ADCSRA  = _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

ISR(TIMER1_OVF_vect) {
  ADCSRA |= _BV(ADSC);
  TIMSK1 &= ~_BV(TOIE1); // wyłącz przerwanie overflow (BOTTOM)
}

ISR(TIMER1_CAPT_vect) {
  ADCSRA |=  _BV(ADSC);
  TIMSK1 &= ~_BV(ICIE1); // wyłącz przerwanie capture (TOP)
}

#define VREF 5.0
#define MAX_READING 1024

float voltage_from_adc(uint16_t adc) {
  return (adc * VREF * 1000) / MAX_READING;
}

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  timer1_init();
  OCR1A = ICR1/2;

  adc_init();
  sei();

  while(1) {
    TIFR1  |= _BV(ICF1);
    TIMSK1 |= _BV(ICIE1); // włącz przerwanie capture (TOP)
    while (!(ADCSRA & _BV(ADIF)));
    ADCSRA |= _BV(ADIF);
    printf("MOSFET zamknięty: %.1fmV\r\n", voltage_from_adc(ADC));

    TIFR1  |= _BV(TOV1);
    TIMSK1 |= _BV(TOIE1); // włącz przerwanie overflow (BOTTOM)
    while (!(ADCSRA & _BV(ADIF)));
    ADCSRA |= _BV(ADIF);
    printf("MOSFET otwarty:   %.1fmV\r\n", voltage_from_adc(ADC));

    _delay_ms(500);
  }
}

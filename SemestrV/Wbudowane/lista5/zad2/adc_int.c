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

void io_init() {
  // ustaw pull-up na PD2 (INT0)
  PORTD |= _BV(PORTD2);
  // ustaw wyzwalanie przerwania na INT0 zboczem opadającym
  EICRA |= _BV(ISC01);
  // odmaskuj przerwania dla INT0
  EIMSK |= _BV(INT0);
}

void adc_init()
{
  ADMUX   = _BV(REFS0); // referencja AVcc, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADIE);  // włącz przerwania ADC
  ADCSRA |= _BV(ADATE); // włącz auto-trigger
  ADCSRB  = _BV(ADTS1); // konwersja przy INT0
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

void timer2_init()
{
  // Preskaler: 1024
  // Top: 0xFF
  // Częstotliwość: około 61 Hz
  TCCR2B = _BV(CS20) | _BV(CS21) | _BV(CS22);
  TIMSK2 |= _BV(TOIE2);
}

volatile uint16_t recorded_value;

ISR(INT0_vect) {
}

ISR(ADC_vect) {
  recorded_value = ADC;
}

ISR(TIMER2_OVF_vect) {
}

# define VIN_MAX 1023
# define R1 10000

float resistance_from_voltage(int voltage) {
  float frac = VIN_MAX / (float)voltage;
  return R1 * (frac - 1);
}

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  adc_init();
  timer2_init();
  io_init();
  set_sleep_mode(SLEEP_MODE_PWR_SAVE);
  sei();
  while(1) {
    sleep_mode();

    cli();
    printf("%.1f\r\n", resistance_from_voltage(recorded_value));
    sei();
    
    _delay_ms(1);
  }
}

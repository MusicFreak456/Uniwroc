#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <math.h>

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

void adc_init() {
  ADMUX   = _BV(REFS1) | _BV(REFS0); // referencja 1.1V, wejście ADC0
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

#define VREF 1.1
#define MAX_READING 1024
#define I 130

float voltage_from_adc(uint16_t adc) {
  return (adc * VREF * 1000) / MAX_READING;
}

float resistance_from_voltage(float voltage) {
  return voltage * 1000 / I;
}

#define B 4126.62
#define EXP_PRECALC 0.0045 // R_0 * e^(-B/T_0)
#define KELVIN_0 273.15

float temperature_from_resistance(float r) {
  float res = r / EXP_PRECALC;
  res = log(res);
  return B / res - KELVIN_0;
}

float temperature;

ISR(ADC_vect) {
  uint16_t measurment = ADC;
  float voltage = voltage_from_adc(measurment);
  float resistance = resistance_from_voltage(voltage);
  temperature = temperature_from_resistance(resistance);
}

ISR(TIMER0_OVF_vect) {
}

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  adc_init();
  timer0_init();

  set_sleep_mode(SLEEP_MODE_IDLE);
  sei();

  while(1) {
    sleep_mode();
    cli();
    printf("Odczytano: %.2fºC\r\n", temperature);
    sei();
  }
}
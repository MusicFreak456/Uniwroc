#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <util/delay.h>
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

void timer1_init() {
  // ustaw tryb licznika
  // WGM1  = 1111 -- fast pwm, top = OCR1A
  // CS1   = 010  -- prescaler 8
  // OCR1A  = 249
  // częstotliwość: 16e6 / (8 * (1+249)) = 8kHz
  TCCR1A = _BV(WGM10) | _BV(WGM11);
  TCCR1B = _BV(WGM12) | _BV(WGM13) | _BV(CS11);
  OCR1A = 249;
  TIMSK1 = _BV(TOIE1);
}

void adc_init() {
  ADMUX   = _BV(REFS0) | 1; // referencja Vin, wejście ADC1
  DIDR0   = _BV(ADC1D); // wyłącz wejście cyfrowe na ADC1
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADIE);  // włącz przerwania ADC
  ADCSRA |= _BV(ADATE); // włącz auto-trigger
  ADCSRB  = _BV(ADTS2) | _BV(ADTS1); // konwersja przy overflow w liczniku 1
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

#define MAX_SAMPLES 100

uint16_t sample_count;
uint16_t sample_buffer[MAX_SAMPLES];
float max_mean;

ISR(TIMER1_OVF_vect) {
}

ISR(ADC_vect) {
  if(sample_count < MAX_SAMPLES){
    sample_buffer[sample_count] = ADC;
    sample_count++;
  }
}

#define VREF 5.0
#define MAX_READING 1024

float voltage_from_adc(uint16_t adc) {
  return (adc * VREF) / MAX_READING - 2.5;
}

float root_mean_squere_of_sample() {
  float sum = 0;
  for(int i = 0; i < sample_count; i++) {
    float voltage = voltage_from_adc(sample_buffer[i]);
    sum += voltage * voltage;
  }
  sum /= sample_count;
  return sqrt(sum);
}

void reset_sampling() {
  sample_count = 0;
}

#define XREF 2.046

void print_samples() {
  printf("\nLiczba próbek: %d\r\n", sample_count);
  for(int i = 0; i < sample_count; i++) {
    printf("%.2f ", voltage_from_adc(sample_buffer[i]));
  }
  printf("\n");
  float mean = root_mean_squere_of_sample();
  if(mean > max_mean) {
    max_mean = mean;
  }
  printf("Mean: %.3f\r\n", mean);
  printf("Max mean: %.3f\r\n", max_mean);
  printf("dBFS: %.4f\r\n", 20 * log10(mean / XREF));
}

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  adc_init();
  timer1_init();

  _delay_ms(10);
  sei();
  while(1) {
    _delay_ms(500);
    cli();
    print_samples();
    reset_sampling();
    sei();
  }
}
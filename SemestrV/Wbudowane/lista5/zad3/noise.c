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

#define NUMBER_OF_SAMPLES 50

void adc_init()
{
  ADMUX   = _BV(REFS0) | 0b1110; // referencja AVcc, wejście 1.1V
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  ADCSRA  = _BV(ADPS0) | _BV(ADPS2); // preskaler 16
  // ADCSRA  = _BV(ADPS0); // preskaler 2
  ADCSRA |= _BV(ADIE);  // włącz przerwania ADC
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

uint16_t clean[NUMBER_OF_SAMPLES];
uint8_t sample_number;

ISR(ADC_vect) {
  clean[sample_number] = ADC;
  sample_number++;
}

float sample_mean(uint16_t *sample, uint8_t n) {
  uint32_t sum = 0;

  for (int i = 0; i < n; i++) {
    sum += sample[i];
  }
  
  return sum / (float)n;
}

float sample_variance(uint16_t *sample, uint8_t n) {
  float mean = sample_mean(sample, n);
  float sum = 0.0;

  for (int i = 0; i < NUMBER_OF_SAMPLES; i++) {
    float diff = sample[i] - mean;
    sum += diff * diff;
  }

  return sum / (n - 1);
}

int main()
{
  uint16_t noisy[NUMBER_OF_SAMPLES];
  
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  adc_init();
  set_sleep_mode(SLEEP_MODE_ADC);

  _delay_ms(20);
  while(1) {
    sei();
    for (int i = 0; i < NUMBER_OF_SAMPLES; i++) {
      sleep_mode();
    }
    cli();

    for (int i = 0; i < NUMBER_OF_SAMPLES; i++) {
      ADCSRA |= _BV(ADSC);
      while (!(ADCSRA & _BV(ADIF)));
      ADCSRA |= _BV(ADIF);    
      noisy[i] = ADC;
    }
    
    printf("clean: %f\r\n", sample_variance(clean, NUMBER_OF_SAMPLES));
    printf("noisy: %f\r\n\n", sample_variance(noisy, NUMBER_OF_SAMPLES));

    sample_number = 0;
    _delay_ms(1000);
  }
}

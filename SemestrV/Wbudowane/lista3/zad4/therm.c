#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>
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

# define VIN_MAX 1023
# define INTERNAL_REF 1.1
# define R1 2200
# define B 4126.62
# define T_0 298.15
# define R_0 4700
# define EXP_PRECALC 0.0045 // R_0 * e^(-B/T_0)
# define KELVIN_0 273.15

void adc_init()
{
  ADMUX   = _BV(REFS0) | ADC4D;
  DIDR0   = _BV(ADC4D);
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); 
  ADCSRA |= _BV(ADEN);
}

float resistance_from_voltage(int voltage) {
    float frac = VIN_MAX / (float)voltage;
    return R1 * (frac - 1);
}

float temperature_from_resistance(float r) {
  float res = r / EXP_PRECALC;
  res = log(res);
  return B / res - KELVIN_0;
}

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  
  adc_init();
  while(1) {
    ADCSRA |= _BV(ADSC);
    while (!(ADCSRA & _BV(ADIF)));
    ADCSRA |= _BV(ADIF);
    uint16_t v = ADC;
    float r2 = resistance_from_voltage(v);
    float temp = temperature_from_resistance(r2);
    printf("Odczytano: %.2f C \r\n", temp);
    _delay_ms(1000);
  }
}

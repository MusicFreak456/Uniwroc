#include <avr/io.h>
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

#define VIN 1.1
#define MAX_READING 1024

#define LED PD6
#define LED_DDR DDRD
#define LED_PORT PORTD

void adc_init()
{
  ADMUX   = _BV(REFS0) | 0b1110;
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2);
  ADCSRA |= _BV(ADEN);
}

float voltage_from_adc(uint16_t adc) {
  return VIN / adc * MAX_READING;
}

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  
  adc_init();
  LED_DDR = _BV(LED);
    
  while(1) {
    ADCSRA |= _BV(ADSC);
    while (!(ADCSRA & _BV(ADIF)));
    ADCSRA |= _BV(ADIF);    
    uint16_t adc = ADC;
    float vin_voltage = voltage_from_adc(adc);
    printf("Odczytano: %.2fV\r\n", vin_voltage);

    LED_PORT ^= _BV(LED);
    _delay_ms(1000);
  }
}

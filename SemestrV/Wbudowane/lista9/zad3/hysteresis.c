#include <avr/io.h>
#include <avr/interrupt.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
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

#define TC 10.0
#define T0 500.0

#define VREF 1.1
#define MAX_READING 1024.0

#define MARGIN_OF_ERROR 1.0

#define HEATER _BV(PB5)
#define HEATER_DDR DDRB
#define HEATER_PORT PORTB

void adc_init()
{
  ADMUX   = _BV(REFS0) | _BV(REFS1); // referencja 1.1V, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADIE);  // włącz przerwania ADC
  ADCSRA |= _BV(ADATE); // włącz auto-trigger
  ADCSRB  = _BV(ADTS2); // konwersja przy overflow w liczniku 0
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

void timer0_init()
{
  // Preskaler: 1024
  // Top: 0xFF
  // Częstotliwość: około 61 Hz
  TCCR0B = _BV(CS02) | _BV(CS00);
  TIMSK0 |= _BV(TOIE0);
}

float measured_temperature;
bool  is_heater_on;
float top_temperature;
float bott_temperature;

void heater_on() {
  HEATER_PORT |= HEATER;
  is_heater_on = true;
}

void heater_off() {
  HEATER_PORT &= ~HEATER;
  is_heater_on = false;
}

float voltage_of_adc(uint16_t adc_reading) {
  return (adc_reading * VREF * 1000) / MAX_READING;
}

float temperature_of_voltage(float voltage) {
  return (voltage - T0) / TC;
}

ISR(ADC_vect) {
  uint16_t measured_value = ADC;
  float voltage = voltage_of_adc(measured_value);
  measured_temperature = temperature_of_voltage(voltage); 

  if(is_heater_on && measured_temperature > top_temperature) {
    heater_off();
  } else if (!is_heater_on && measured_temperature < bott_temperature) {
    heater_on();
  }
}

ISR(TIMER0_OVF_vect) {
}

#define MAX_LINE_SIZE 15

void set_new_temperature(char *line) {
  float new_value;
  if(sscanf(line, "%f", &new_value) > 0) {
    cli();
    top_temperature = new_value  + MARGIN_OF_ERROR;
    bott_temperature = new_value - MARGIN_OF_ERROR;
    sei();
  } else {
    printf("No value given\r\n");
  }
}

void show_stats() {
  printf("Measured temperature: %.2f\r\n", measured_temperature);
  printf("Set temperature: %.2f\r\n", top_temperature - MARGIN_OF_ERROR);
  printf("Is heater on: %s\r\n", is_heater_on? "yes" : "no");
}

void interpret(char* line) {
  char command[MAX_LINE_SIZE];
  sscanf(line, "%s", command);
  line += strlen(command) + 1;

  if( strcmp(command, "show") == 0 ) {
    show_stats();
  } else if( strcmp(command, "set") == 0 ) {
    set_new_temperature(line);
  } else {
    printf("Uknown command\r\n");
  }
}

int main()
{
  HEATER_DDR  |= HEATER;
  HEATER_PORT &= ~HEATER;

  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  adc_init();
  timer0_init();
  top_temperature = 0 + MARGIN_OF_ERROR;
  bott_temperature = 0 - MARGIN_OF_ERROR;
  is_heater_on = false;
  char line[MAX_LINE_SIZE];

  sei();
  while(1) {
    printf("$ ");
    fgets(line, MAX_LINE_SIZE, stdin);
    interpret(line);
  }
}

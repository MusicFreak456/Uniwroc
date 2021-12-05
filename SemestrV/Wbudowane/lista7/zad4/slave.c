#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>
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


void spi_init() {
  // włącz SPI w trybie slave
  SPCR = _BV(SPE);
  // włącz przerwania SPI
  SPCR |= _BV(SPIE) | _BV(DORD);
}

#define CLK_OUT _BV(PC0)
#define MOSI    _BV(PC1)
#define MASTER_DDR  DDRC
#define MASTER_PORT PORTC

volatile uint8_t sent;
volatile uint8_t received;
volatile uint8_t value;

ISR(SPI_STC_vect) {
  value = SPDR;
  received = 1;
}

void transmit_byte(int8_t byte) {
  for(int i = 0; i < 8; i++) {
    if(byte & 1) MASTER_PORT |= MOSI;
    else MASTER_PORT &= ~MOSI;
    byte >>= 1;
    _delay_ms(1);

    MASTER_PORT = CLK_OUT;
    _delay_ms(1);
    MASTER_PORT = ~CLK_OUT;
  }
}


int main() {
  MASTER_DDR  = CLK_OUT | MOSI;
  MASTER_PORT &= ~CLK_OUT;

  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  set_sleep_mode(SLEEP_MODE_IDLE);

  spi_init();
  sei();
  uint8_t i = 0;

  while (1) {
    if(!sent){
      transmit_byte(i);
      printf("Transmited: %d\n\r",i);
      i++;
      sent = 1;
    } else if (received) {
      printf("Received: %d\n\r", value);
      sent = 0;
      received = 0;
    }
  }
}
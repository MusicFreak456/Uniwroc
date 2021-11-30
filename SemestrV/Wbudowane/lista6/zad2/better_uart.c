#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>
#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>
#include <util/delay.h>

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

#define BUFFER_SIZE 255

typedef uint8_t buffer_size_t;
typedef char buffer_elem_t;
typedef struct {
  buffer_size_t read_index;
  buffer_size_t write_index;
  buffer_size_t number_of_elems;
  buffer_elem_t array[BUFFER_SIZE];
} cyclic_buffer;

volatile cyclic_buffer read_buffer;
volatile cyclic_buffer write_buffer;
volatile bool write_done;

buffer_size_t next(buffer_size_t index) {
  buffer_size_t index_incr = index + 1;
  return (index_incr == BUFFER_SIZE) ? 0 : index_incr;
}

bool append(volatile cyclic_buffer *buffer, buffer_elem_t elem) {
  if( buffer->number_of_elems == BUFFER_SIZE ) return false;

  buffer_size_t write_index = buffer->write_index;
  buffer->array[write_index] = elem;
  buffer->write_index = next(write_index);
  buffer->number_of_elems++;
  return true;
}

bool consume(volatile cyclic_buffer *buffer, buffer_elem_t *elem) {
  if( buffer->number_of_elems == 0 ) return false;

  buffer_size_t read_index = buffer->read_index;
  *elem = buffer->array[read_index];
  buffer->read_index = next(read_index);
  buffer->number_of_elems--;
  return true;
}

// inicjalizacja UART
void uart_init() {
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
  // wyczyść rejestr UCSR0A
  UCSR0A = 0;
  // włącz odbiornik i nadajnik
  UCSR0B  = _BV(RXEN0) | _BV(TXEN0);
  UCSR0B |= _BV(RXCIE0);
  // ustaw format 8n1
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);

  write_done = true;
}

ISR(USART_UDRE_vect) {
  buffer_elem_t character;
  if(consume(&write_buffer, &character)){
    UDR0 = character;
  } else {
    write_done = true;
    UCSR0B &= ~_BV(UDRIE0);
  }
}

ISR(USART_RX_vect) {
  char new_char = UDR0;
  append(&read_buffer, new_char);
}

// transmisja jednego znaku
int uart_transmit(char data, FILE *stream) {
  UCSR0B &= ~_BV(UDRIE0);

  if(write_done) {
    write_done = false;
    UDR0 = data;
  } else while (!append(&write_buffer, data)) {
    UCSR0B |= _BV(UDRIE0);
    UCSR0B &= ~_BV(UDRIE0);
  }
  UCSR0B |= _BV(UDRIE0);

  return 0;
}

// odczyt jednego znaku
int uart_receive(FILE *stream) {
  char new_character;
  UCSR0B &= ~_BV(RXCIE0);
  while (!consume(&read_buffer, &new_character)) {
    UCSR0B |= _BV(RXCIE0);
    UCSR0B &= ~_BV(RXCIE0);
  }
  UCSR0B |= _BV(RXCIE0);
  return new_character;
}

FILE uart_file;

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  set_sleep_mode(SLEEP_MODE_IDLE);
  sei();

  while (1){
    int32_t a, b;
    printf("Enter two numbers separated by space:\r\n");
    // scanf("%"SCNd32" %"SCNd32, &a, &b);
    // printf("Odczytano: %"PRId32" %"PRId32"\r\n",a,b);
  }
}
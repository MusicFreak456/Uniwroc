#include <avr/io.h>
#include <avr/interrupt.h>
#include <stdio.h>
#include <stdbool.h>

#include "FreeRTOS.h"
#include "task.h"
#include "uart.h"
#include "queue.h"

#if(INCLUDE_vTaskSuspend == 0)
  #error "This implementation of uart requires INCLUDE_vTaskSuspend to be set to 1"
#endif

#ifndef F_CPU
  #define F_CPU 16000000UL
#endif

#ifndef BAUD
  #define BAUD 9600
#endif

#ifndef UART_BUFFER_SIZE
  #define UART_BUFFER_SIZE 20
#endif

#include <util/setbaud.h>

int uart_transmit(char c, FILE *stream);
int uart_receive(FILE *stream);

FILE uart_file = FDEV_SETUP_STREAM(uart_transmit, uart_receive, _FDEV_SETUP_RW);

volatile static bool write_done;

static StaticQueue_t xStaticReadQueue;
static QueueHandle_t xReadQueue;
uint8_t ucReadQueueStorage[ sizeof(char) * UART_BUFFER_SIZE ];

static StaticQueue_t xStaticWriteQueue;
static QueueHandle_t xWriteQueue;
uint8_t ucWriteQueueStorage[ sizeof(char) * UART_BUFFER_SIZE ];

void uart_init() {
  UBRR0H = UBRRH_VALUE;
  UBRR0L = UBRRL_VALUE;
#if USE_2X
  UCSR0A |= _BV(U2X0);
#else
  UCSR0A &= ~(_BV(U2X0));
#endif
  UCSR0C = _BV(UCSZ01) | _BV(UCSZ00); /* 8-bit data */
  UCSR0B = _BV(RXEN0) | _BV(TXEN0); /* Enable RX and TX */
  UCSR0B |= _BV(RXCIE0); /* Enable interrupts */

  DDRB |= _BV(PB5);

  xReadQueue = xQueueCreateStatic(
    UART_BUFFER_SIZE,
    sizeof(char),
    ucReadQueueStorage,
    &xStaticReadQueue
  );

  xWriteQueue = xQueueCreateStatic(
    UART_BUFFER_SIZE,
    sizeof(char),
    ucWriteQueueStorage,
    &xStaticWriteQueue
  );

  write_done = true;
}

ISR(USART_UDRE_vect) {
  char character;

  if( xQueueIsQueueEmptyFromISR(xWriteQueue) == pdFALSE ) {
    xQueueReceiveFromISR(xWriteQueue, &character, NULL);
    UDR0 = character;
  } else {
    write_done = true;
    UCSR0B &= ~_BV(UDRIE0);
  }
}

ISR(USART_RX_vect) {
  char new_char = UDR0;
  xQueueSendFromISR(xReadQueue, &new_char, NULL);
}

int uart_transmit(char c, FILE *stream) {
  char to_be_transmited = c;

  UCSR0B &= ~_BV(UDRIE0);

  if(write_done) {
    write_done = false;
    UDR0 = to_be_transmited;
    UCSR0B |= _BV(UDRIE0);
    return 0;
  }
  UCSR0B |= _BV(UDRIE0);
  xQueueSend(xWriteQueue, &to_be_transmited, portMAX_DELAY);

  return 0;
}

int uart_receive(FILE *stream) {
  char received;
  xQueueReceive(xReadQueue, &received, portMAX_DELAY);
  return received;
}


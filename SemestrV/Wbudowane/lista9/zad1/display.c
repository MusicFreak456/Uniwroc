#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>
#include "hd44780.h"

/***************** I/O initialization ****************************************/

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

void uart_init()
{
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
  // wyczyść rejestr UCSR0A
  UCSR0A = 0;
  // włącz odbiornik
  UCSR0B = _BV(RXEN0);
  // ustaw format 8n1
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

int uart_receive(FILE *stream)
{
  // czekaj aż znak dostępny
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

FILE uart_file;

int hd44780_transmit(char data, FILE *stream)
{
  LCD_WriteData(data);
  return 0;
}

FILE hd44780_file;

/*****************************************************************************/

#define FIRST_LINE  0
#define SECOND_LINE 1
#define LINE_START  0
#define LINE_WIDTH 16
/* ASCI codes for functional characters */
#define BACKSPACE        8
#define NEWLINE         10
#define CARRIAGE_RETURN 13

int cursor_position;
char line_buffer[16];

void scroll_up() {
  LCD_Clear();
  LCD_Home();
  printf("%s", line_buffer);
  LCD_GoTo(LINE_START, SECOND_LINE);
  cursor_position = LINE_START;
  line_buffer[LINE_START] = '\0';
}

void handle_regular_char(char input) {
  putchar(input);
  line_buffer[cursor_position] = input;
  line_buffer[cursor_position + 1] = '\0';
  cursor_position += 1;
  if(cursor_position == LINE_WIDTH)
   scroll_up();
}

void handle_backspace() {
  if(cursor_position < 1) return;
  cursor_position-=1;
  LCD_GoTo(cursor_position, SECOND_LINE);
  putchar(' ');
  LCD_GoTo(cursor_position, SECOND_LINE);
}

void handle_carriage_return() {
  LCD_GoTo(LINE_START, SECOND_LINE);
}

void handle_input(char input) {
  switch (input)
  {
  case BACKSPACE:
    handle_backspace();
    break;
  case CARRIAGE_RETURN:
    handle_carriage_return();
    break;
  case NEWLINE:
    scroll_up();
    break;
  default:
    handle_regular_char(input);
    break;
  }
}

int main()
{
  /* UART initialization */
  uart_init();
  fdev_setup_stream(&uart_file, NULL, uart_receive, _FDEV_SETUP_READ);
  stdin = &uart_file;

  /* Display initialization */
  LCD_Initialize();
  LCD_Clear();
  fdev_setup_stream(&hd44780_file, hd44780_transmit, NULL, _FDEV_SETUP_WRITE);
  stdout = stderr = &hd44780_file;
  LCD_GoTo(LINE_START, SECOND_LINE);
  LCD_WriteCommand(HD44780_DISPLAY_ONOFF | HD44780_DISPLAY_ON | 
                   HD44780_CURSOR_ON);

  /* Global state initialization */  
  cursor_position = LINE_START;
  char input;

  /* Main loop */
  while(1) {
    input = getchar();
    handle_input(input);
  }
}

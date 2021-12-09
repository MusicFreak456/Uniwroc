#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "i2c.h"

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
int uart_receive(FILE *stream) {
  // czekaj aż znak dostępny
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

FILE uart_file;

#define i2cCheck(code, msg, cont) \
  if ((TWSR & 0xf8) != (code)) { \
    printf(msg " failed, status: %.2x\r\n", TWSR & 0xf8); \
    i2cReset(); \
    cont; \
  }

#define IS_EMPTY_STR(str) (str == NULL)
#define check_ptr(ptr) \
  if(!ptr || ptr[0] == '\0') { \
    return false; \
  } 

/**********************************************************/

#define MAX_LINE_SIZE 50
#define MAX_ARGUMENT_SIZE 10
#define MAX_ARGUMENT_NUMBER 2

#define int_of_char(character) ((character) - 48)
#define char_of_digit(digit) ((digit) + 48)

uint8_t compose(char char1, char char2) {
  return ((int_of_char(char1) << 4) | int_of_char(char2));
}

void decompose_print(uint8_t sm) {
  printf("%c%c", char_of_digit(sm >> 4), char_of_digit(sm & 0xf));
}

typedef struct {
  uint8_t day;
  uint8_t month;
  uint8_t year;
} cl_date_t;

typedef struct {
  uint8_t second;
  uint8_t minute;
  uint8_t hour;
} cl_time_t;

typedef enum {
  T_NULL = 0,
  T_GET,
  T_SET,
  T_DATE,
  T_TIME
} command;

typedef union {
  command cmd;
  cl_date_t date;
  cl_time_t time;
} token_t;

bool date_of_string(char *str, cl_date_t *date) {
  if(IS_EMPTY_STR(str)) return false;

  char *dash = strchr(str, '-');
  check_ptr(dash)
  date->day = compose(str[0], str[1]);

  str = dash + 1;
  dash = strchr(str, '-');
  check_ptr(dash);
  date->month = compose(str[0], str[1]);

  str = dash + 1;
  check_ptr(str);
  date->year = compose(str[2], str[3]);

  return true;
}

bool time_of_string(char *str, cl_time_t *time) {
  if(IS_EMPTY_STR(str)) return false;

  char *dash = strchr(str, ':');
  check_ptr(dash)
  time->hour = compose(str[0], str[1]);

  str = dash + 1;
  dash = strchr(str, ':');
  check_ptr(dash);
  time->minute = compose(str[0], str[1]);

  str = dash + 1;
  check_ptr(str);
  time->second = compose(str[0], str[1]);

  return true;
}

const uint8_t timer_address = 0b11010000;

#define SECONDS_REGISTER 0x00
#define DATE_REGISTER 0x04

void start_write(uint8_t addr) {
  i2cStart();
  i2cCheck(0x08, "I2C start", return)
  i2cSend(timer_address);
  i2cCheck(0x18, "I2C timer write request", return)
  i2cSend(addr);
  i2cCheck(0x28, "I2C timer set address", return)
}

void byte_write(uint8_t data) {
  i2cSend(data);
  i2cCheck(0x28, "I2C timer send data", return)
}

void set_time(cl_time_t *time) {
  start_write(SECONDS_REGISTER);

  byte_write(time->second);
  byte_write(time->minute);
  byte_write(time->hour);

  i2cStop();
  i2cCheck(0xf8, "I2C stop", return)
}

void set_date(cl_date_t *date) {
  start_write(DATE_REGISTER);

  byte_write(date->day);
  byte_write(date->month);
  byte_write(date->year);

  i2cStop();
  i2cCheck(0xf8, "I2C stop", return)
}

void exec_set(command option, token_t *time_or_date , uint8_t args_num) {
  if(args_num != 2) {
    printf("Bad usage\r\n");
    return;
  }

  switch (option) {
  case T_TIME:
    set_time(&time_or_date->time);
    break;
  case T_DATE:
    set_date(&time_or_date->date);
    break;
  default:
    printf("Uknown command\r\n"); 
    break;
  }
}

uint8_t read_byte(bool ack) {
  uint8_t data;
  if(ack) {
    data = i2cReadAck(); 
    i2cCheck(0x50, "I2C EEPROM read ack", return 0)
  }
  else {
    data = i2cReadNoAck();
    i2cCheck(0x58, "I2C EEPROM read no ack", return 0)
  }
  return data;
}

void start_read(uint8_t addr) {
  i2cStart();
  i2cCheck(0x08, "I2C start", return)
  i2cSend(timer_address);
  i2cCheck(0x18, "I2C timer write request", return)
  i2cSend(addr);
  i2cCheck(0x28, "I2C timer set address", return)
  i2cStart();
  i2cCheck(0x10, "I2C repeated start", return)
  i2cSend(timer_address | 0x1);
  i2cCheck(0x40, "I2C timer read request", return)
}

void get_time() {
  cl_time_t time; 

  start_read(SECONDS_REGISTER);
  time.second = read_byte(true);
  time.minute = read_byte(true);
  time.hour   = read_byte(false) & 0x3F;
  i2cStop();
  i2cCheck(0xf8, "I2C stop", return)

  decompose_print(time.hour);
  printf(":");
  decompose_print(time.minute);
  printf(":");
  decompose_print(time.second);
  printf("\r\n");
}

void get_date() {
  cl_date_t date; 

  start_read(DATE_REGISTER);
  date.day   = read_byte(true);
  date.month = read_byte(true) & 0x7F;
  date.year  = read_byte(false);
  i2cStop();
  i2cCheck(0xf8, "I2C stop", return)

  decompose_print(date.day);
  printf("-");
  decompose_print(date.month);
  printf("-20");
  decompose_print(date.year);
  printf("\r\n");
}

void exec_get(command option, uint8_t args_num) {
  if(args_num != 0) {
    printf("Bad usage\r\n");
    return;
  }

  switch (option) {
  case T_TIME:
    get_time();
    break;
  case T_DATE:
    get_date();
    break;
  default:
    printf("Uknown command\r\n");
    break;
  }
}

uint8_t tokenize(char *cmd, token_t *tokens) {
  char command[MAX_ARGUMENT_SIZE];
  uint8_t num_of_args = sscanf(cmd, "%s", command) < 0 ? 0 : 1;
  *strchr(cmd, '\r') = '\0';

  if( strcmp(command, "set") == 0 ) {
    tokens[0].cmd = T_SET;
    cmd += strlen(command) + 1;
  } else {
    tokens[0].cmd = T_GET;
  }

  char *arg1 = cmd;
  char *arg2 = strchr(cmd, ' ');
  if(arg2) { *arg2 = '\0'; arg2++; }
  num_of_args += (*arg1 != '\0') + (arg2 != NULL);

  if( strcmp(arg1, "time") == 0 ) {
    tokens[1].cmd = T_TIME;
    num_of_args -= !time_of_string(arg2, &tokens[2].time);
  } else if( strcmp(arg1, "date") == 0 ) {
    tokens[1].cmd = T_DATE;
    num_of_args -= !date_of_string(arg2, &tokens[2].date);
  } 

  return num_of_args;
}

void exec(char *cmd) {
  token_t tokens[MAX_ARGUMENT_NUMBER + 1] = {T_NULL};
  uint8_t args_num = tokenize(cmd, tokens) - 1;

  switch (tokens[0].cmd) {
  case T_GET:
    exec_get(tokens[1].cmd, args_num);
    break;
  case T_SET:
    exec_set(tokens[1].cmd, &tokens[2], args_num);
    break;
  default:
    printf("Unknown command\r\n");
    break;
  }
}

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  i2cInit();

  char line[MAX_LINE_SIZE];

  while (1) {
    printf("$ ");
    fgets(line, MAX_LINE_SIZE, stdin);
    exec(line);
  }
}
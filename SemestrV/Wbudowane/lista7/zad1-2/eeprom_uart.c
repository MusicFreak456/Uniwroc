#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include "i2c.h"

#define MAX_LINE_SIZE 50
#define MAX_ARGUMENT_SIZE 5
#define MAX_ARGUMENT_NUMBER 2
#define MAX_BYTES 255
#define MAX_I8_LINE 522
// maximum programming time z datasheeta
#define PROGRAMMING_TIME _delay_ms(20);

typedef uint16_t token_t;

enum {
  T_NULL,
  T_READ,
  T_WRITE
};

typedef enum {
  I8_DATA = 0,
  I8_EOF
} record_type;

typedef struct {
  uint16_t address;
  record_type record_type;
  uint8_t byte_count;
  uint8_t checksum;
  uint8_t data[MAX_BYTES];
} I8HEX_t;

#define BLOCK_NR(addr) ((addr & 0x100) >> 7)

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

#define i2cCheck(code, msg, cont) \
  if ((TWSR & 0xf8) != (code)) { \
    printf(msg " failed, status: %.2x\r\n", TWSR & 0xf8); \
    i2cReset(); \
    cont; \
  }

const uint8_t eeprom_addr = 0xa0;

void start_write(uint16_t addr) {
  i2cStart();
  i2cCheck(0x08, "I2C start", return)
  i2cSend(eeprom_addr | BLOCK_NR(addr));
  i2cCheck(0x18, "I2C EEPROM write request", return)
  i2cSend(addr & 0xff);
  i2cCheck(0x28, "I2C EEPROM set address", return)
}

void write_single(uint8_t addr, uint8_t value) {
  start_write(addr);
  i2cSend(value);
  i2cCheck(0x28, "I2C EEPROM send data", return)
  i2cStop();
  i2cCheck(0xf8, "I2C stop", return)
  PROGRAMMING_TIME
  printf("%.3x: %x\r\n", addr, value);
}

// :0300000001055A9D
// :0300000002055A9C
// :0300000001065A9C
// :0500000001065A00009A
// :00000001FF
// :0200FF00F322EA
// :0200FF00F222EB

void write_burst(uint16_t addr, uint8_t *data , uint8_t len) {
  uint8_t block_nr = BLOCK_NR(addr);

  start_write(addr);
  for (int i = 0; i < len; i++) {
    i2cSend(data[i]);
    i2cCheck(0x28, "I2C EEPROM send data", return)

    uint16_t next_addr = addr + 1;
    if(block_nr != BLOCK_NR(next_addr)) {
      if(i != len - 1) {
        i2cStop();
        i2cCheck(0xf8, "I2C stop", return)
        PROGRAMMING_TIME
        start_write(next_addr);
      }
      block_nr = BLOCK_NR(next_addr);
    }

    addr = next_addr;
  }
  
  i2cStop();
  i2cCheck(0xf8, "I2C stop", return);
}

void write_seq(I8HEX_t *record) {
  uint8_t num_of_bytes = record->byte_count;
  uint16_t addr = record->address;
  uint8_t num_of_bursts = 1 + ((num_of_bytes - 1) / 4);

  for (int i = 0; i < num_of_bursts; i++) {
    uint8_t length = i == num_of_bursts - 1 ? num_of_bytes - i * 4 : 4;
    write_burst(addr, record->data + i * 4, length);
    PROGRAMMING_TIME
    addr += 4;
  }

}

#define B_CNT_LEN 3
#define ADDR_LEN  5
#define REC_T_LEN 3

uint8_t get_checksum(I8HEX_t *record) {
  uint8_t sum = 0;
  sum += record->byte_count;
  sum += record->address & 0xFF;
  sum += (record->address >> 8) & 0xFF;
  sum += record->record_type;

  uint8_t num_of_elems = record->byte_count;
  for(int i = 0; i < num_of_elems; i++) {
    sum += record->data[i];
  }

  return ~sum + 1;
}

bool i8hex_of_input(I8HEX_t *record) {
  getchar();

  char buffer[5];
  fgets(buffer, B_CNT_LEN, stdin);
  record->byte_count = (uint8_t) strtol(buffer, NULL, 16);

  fgets(buffer, ADDR_LEN, stdin);
  record->address = (uint16_t) strtol(buffer, NULL, 16);

  fgets(buffer, REC_T_LEN, stdin);
  record->record_type = (record_type) strtol(buffer, NULL, 16);

  uint8_t num_of_elems = record->byte_count;
  for (int i = 0; i < num_of_elems; i++) {
    fgets(buffer, 3, stdin);
    record->data[i] = (uint8_t) strtol(buffer, NULL, 16);
  }

  fgets(buffer, 3, stdin);
  record->checksum = (uint8_t) strtol(buffer, NULL, 16);

  printf("\r\n");
  return record->checksum == get_checksum(record);
}

void exec_seq_write() {
  I8HEX_t record;
  while(1){
    if(i8hex_of_input(&record)) {
      if(record.record_type == I8_DATA){
        write_seq(&record);
      } else if (record.record_type == I8_EOF) {
        break;
      }
    } else {
      printf("Checksum mismatch\r\n");
      break;
    }
  }
}

void exec_write(uint16_t addr, uint8_t value, uint8_t args_num) {
  switch (args_num) {
  case 0:
    exec_seq_write();
    break;
  case 2:
    write_single(addr, value);
    break;
  default:
    printf("Bad usage\r\n");
    break;
  }
}

void start_read(uint16_t addr, bool repeated) {
  i2cStart();
  i2cCheck((repeated? 0x10 : 0x08), "I2C start", return)
  i2cSend(eeprom_addr | BLOCK_NR(addr));
  i2cCheck(0x18, "I2C EEPROM write request", return)
  i2cSend(addr & 0xff);
  i2cCheck(0x28, "I2C EEPROM set address", return)
  i2cStart();
  i2cCheck(0x10, "I2C second start", return)
  i2cSend(eeprom_addr | 0x1 | BLOCK_NR(addr));
  i2cCheck(0x40, "I2C EEPROM read request", return) 
}

void read_single(uint16_t addr) {
  start_read(addr, false);
  uint8_t data = i2cReadNoAck();
  i2cCheck(0x58, "I2C EEPROM read", return)
  i2cStop();
  i2cCheck(0xf8, "I2C stop", return)
  printf("%.3x: %x\r\n", addr, data);
}

uint8_t print_byte(bool ack) {
  uint8_t data;
  if(ack) {
    data = i2cReadAck(); 
    i2cCheck(0x50, "I2C EEPROM read ack", return 0)
  }
  else {
    data = i2cReadNoAck();
    i2cCheck(0x58, "I2C EEPROM read no ack", return 0)
  }
  printf("%.2X", data);
  return data;
}

uint8_t print_n_bytes(uint16_t addr, uint8_t n, bool repeated) {
  uint8_t block_nr = BLOCK_NR(addr);
  uint8_t checksum = 0;

  start_read(addr, repeated);
  for (uint8_t i = 0; i < n; i++){
    uint16_t next_addr = addr + 1;

    if(block_nr != BLOCK_NR(next_addr)) {
      checksum += print_byte(false);
      if(i != n-1)
        start_read(next_addr, true);
      addr = next_addr;
      block_nr = BLOCK_NR(next_addr);
      continue;
    }

    if(i == n - 1)
      checksum += print_byte(false);
    else
      checksum += print_byte(true);

    addr = next_addr;
  }

  return checksum;
}

uint8_t checksum_header(uint8_t byte_count, uint16_t addr, record_type r_type) {
  return byte_count + (addr & 0xFF) + (addr >> 8) + r_type;
}

void read_seq(uint16_t addr, uint16_t length) {
  if(length > 512 || length + addr > 0x1ff) {
    printf("To many bytes to read\r\n");
    return;
  } else if (length == 0) {
    return;
  }
  
  uint8_t num_of_hexs = 1 + ((length - 1) / 255);

  for (int i = 0; i < num_of_hexs; i++) {
    uint8_t checksum = 0;
    uint8_t hex_length = i == num_of_hexs - 1 ? length - i * 255 : 255;
    printf(":%.2X%.4X%.2X", hex_length, addr, I8_DATA);
    checksum += checksum_header(length, addr, I8_DATA);
    checksum += print_n_bytes(addr, hex_length, i != 0);
    printf("%.2X", (uint8_t)(~checksum + 1));
    printf("\r\n");
    addr += 255;
  }

  i2cStop();
  i2cCheck(0xf8, "I2C stop", return)
}

void exec_read(uint16_t addr, uint16_t length, uint8_t args_num) {
  switch (args_num) {
  case 1:
    read_single(addr);
    break;
  case 2:
    read_seq(addr, length);
    break;
  default:
    printf("Bad usage \r\n");
    break;
  }
}

uint8_t tokenize(char *cmd, token_t *tokens) {
  char command[MAX_ARGUMENT_SIZE];
  sscanf(cmd, "%s", command);

  if( strcmp(command, "write") == 0 )
    tokens[0] = T_WRITE;
  else if ( strcmp(command, "read") == 0 ) 
    tokens[0] = T_READ;

  cmd += strlen(command) + 1;

  return sscanf(cmd, "%"SCNx16" %"SCNu16, &tokens[1], &tokens[2]) + 1;
}

void exec(char *cmd) {
  token_t tokens[MAX_ARGUMENT_NUMBER + 1] = {T_NULL};
  uint8_t args_num = tokenize(cmd, tokens);
  args_num = args_num == 0 ? 0 : args_num - 1;

  switch (tokens[0]) {
  case T_WRITE:
    exec_write((uint16_t)tokens[1], (uint8_t)tokens[2], args_num);
    break;
  case T_READ:
    exec_read ((uint16_t)tokens[1], (uint16_t)tokens[2], args_num);
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

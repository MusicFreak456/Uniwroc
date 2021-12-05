#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>

#include <assert.h>

#define MAX_LINE_SIZE 50
#define MAX_ARGUMENT_SIZE 5
#define MAX_ARGUMENT_NUMBER 2
#define MAX_BYTES 255

typedef uint16_t token_t;

typedef enum {
  I8_DATA = 0,
  I8_EOF
} record_type;

struct I8HEX_t {
  uint16_t address;
  record_type record_type;
  uint8_t byte_count;
  uint8_t checksum;
  uint8_t data[MAX_BYTES];
};

enum {
  T_NULL,
  T_READ,
  T_WRITE
};

#define B_CNT_LEN 3
#define ADDR_LEN  5
#define REC_T_LEN 3

uint8_t get_checksum(struct I8HEX_t *record) {
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

bool i8hex_of_string(struct I8HEX_t *record) {
  getchar();
  long int data;

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

  return record->checksum == get_checksum(record);
}

void write_single(uint16_t addr, uint8_t value) {
  printf("[performing byte write operation]\r\n");
  printf("addr: 0x%"PRIx16"\r\n", addr);
  printf("value: 0x%"PRIx8"\r\n", value);
}

void exec_write(uint16_t addr, uint8_t value, uint8_t args_num) {
  switch (args_num) {
  case 0:
    printf("Not implemented\r\n");
    break;
  case 2:
    write_single(addr, value);
    break;
  default:
    printf("Bad usage\r\n");
    break;
  }
}

void read_single(uint16_t addr) {
  printf("[performing byte read operation]\r\n");
  printf("addr: 0x%"PRIx16"\r\n", addr);
}

void read_seq(uint16_t addr, uint8_t length, struct I8HEX_t *record) {

}

void exec_read(uint16_t addr, uint8_t length, uint8_t args_num) {
  switch (args_num) {
  case 1:
    read_single(addr);
    break;
  case 2: {
    struct I8HEX_t record;
    read_seq(addr, length, &record);
    break;
  }
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

  return sscanf(cmd, "%"SCNx16" %"SCNx16, &tokens[1], &tokens[2]) + 1;
}

void exec(char *cmd) {
  token_t tokens[MAX_ARGUMENT_NUMBER + 1] = {T_NULL};
  uint8_t args_num = tokenize(cmd, tokens);
  args_num = args_num == 0? 0 : args_num - 1;

  switch (tokens[0]) {
  case T_WRITE:
    exec_write((uint16_t)tokens[1], (uint8_t)tokens[2], args_num);
    break;
  case T_READ:
    exec_read ((uint16_t)tokens[1], (uint8_t)tokens[2], args_num);
    break;
  default:
    printf("Unknown command\n");
    break;
  }
}

int main() {
  // struct I8HEX_t test;
  // i8hex_of_string(&test);

  // printf("Byte count: %d\n", test.byte_count);
  // printf("Address: %d\n", test.address);
  // printf("Record type: %d\n", test.record_type);

  // for(int i = 0; i < test.byte_count; i++) {
  //   printf("%.2x\n", test.data[i]);
  // }

  // printf("Checksum: %.2X\n", test.checksum);
  // printf("Checksum: %.2X\n", get_checksum(&test));

  char line[MAX_LINE_SIZE];

  while (1) {
    printf("$ ");
    fgets(line, MAX_LINE_SIZE, stdin);
    exec(line);
  }
  
  return 0;
}
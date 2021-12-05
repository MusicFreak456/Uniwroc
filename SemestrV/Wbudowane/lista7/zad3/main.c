#include <stdio.h>  
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>

#define MAX_LINE_SIZE 50
#define MAX_ARGUMENT_SIZE 10
#define MAX_ARGUMENT_NUMBER 2

// set date 01-12-2021

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

#define IS_EMPTY_STR(str) (str == NULL)
#define check_ptr(ptr) \
  if(!ptr || ptr[0] == '\0') { \
    return false; \
  } 

#define int_of_char(character) ((character) - 48)
#define char_of_digit(digit) ((digit) + 48)

uint8_t compose(char char1, char char2) {
  return ((int_of_char(char1) << 4) | int_of_char(char2));
}

void decompose_print(uint8_t sm) {
  printf("%c%c", char_of_digit(sm >> 4), char_of_digit(sm & 0xf));
}

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

uint8_t tokenize(char *cmd, token_t *tokens) {
  char command[MAX_ARGUMENT_SIZE];
  uint8_t num_of_args = sscanf(cmd, "%s", command) < 0 ? 0 : 1;
  *strchr(cmd, '\n') = '\0';

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

  if( strcmp(arg1, "date") == 0 ) {
    tokens[1].cmd = T_DATE;
    num_of_args -= !date_of_string(arg2, &tokens[2].date);
  } else if ( strcmp(arg1, "time") == 0 ) {
    tokens[1].cmd = T_TIME;
    num_of_args -= !time_of_string(arg2, &tokens[2].time);
  } 

  return num_of_args;
}

void get_time() {
  printf("[executing get time]\r\n");
}

void get_date() {
  printf("[executing get date]\r\n");
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

void set_time(cl_time_t *time) {
  printf("[executing set time]\r\n");
  printf("hour: ");
  decompose_print(time->hour);
  printf("\r\n");
  printf("minute: ");
  decompose_print(time->minute);
  printf("\r\n");
  printf("second: ");
  decompose_print(time->second);
  printf("\r\n");
}

void set_date(cl_date_t *date) {
  printf("[executing set date]\r\n");
  printf("day: ");
  decompose_print(date->day);
  printf("\r\n");
  printf("month: ");
  decompose_print(date->month);
  printf("\r\n");
  printf("year: 20");
  decompose_print(date->year);
  printf("\r\n");
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
  char line[MAX_LINE_SIZE];

  while (1) {
    printf("$ ");
    fgets(line, MAX_LINE_SIZE, stdin);
    exec(line);
  }
  
  return 0;
}
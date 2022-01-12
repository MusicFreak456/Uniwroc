#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>
#include "hd44780.h"

/***************** I/O initialization ****************************************/

int hd44780_transmit(char data, FILE *stream)
{
  LCD_WriteData(data);
  return 0;
}

FILE hd44780_file;

/*****************************************************************************/

#define DDRAM_INIT_ADDR 0x00
#define CGRAM_INIT_ADDR 0x00
#define MAX_PROGRESS 80
#define NO_PROGRESS 0
#define LINE_START 0
#define FIRST_LINE  0
#define SECOND_LINE 1

typedef enum {
  STAGE_0 = 0,
  STAGE_1,
  STAGE_2,
  STAGE_3,
  STAGE_4,
  STAGE_5
} stage_t;

int progress;
int cursor_position;
stage_t current_stage;

uint8_t stage_symbol_codes[] = {5,4,3,2,1,0};

void upload_characters() {
  LCD_WriteCommand(HD44780_CGRAM_SET | CGRAM_INIT_ADDR);
  for(int i = 0; i < 6; i++) {
    uint8_t mask = 0b11111 << i;
    for(int j = 0; j < 8; j++) {
      LCD_WriteData(mask);
    }
  }
  LCD_WriteCommand(HD44780_DDRAM_SET | DDRAM_INIT_ADDR);
}

void replace_last_character(char new_val) {
  LCD_GoTo(cursor_position - 1, FIRST_LINE);
  putchar(new_val);
}

void initialize_progress_bar() {
  LCD_Clear();
  LCD_Home();
  upload_characters();
  putchar(STAGE_0);
  cursor_position = LINE_START + 1;
  current_stage = STAGE_0;
}

uint8_t next_step() {
  if(cursor_position > 15 && current_stage == STAGE_5) return 0;
  if(current_stage == STAGE_5) {
    putchar(stage_symbol_codes[STAGE_1]);
    cursor_position++;
    current_stage = STAGE_1;
  } else {
    replace_last_character(stage_symbol_codes[current_stage+1]);
    current_stage++;
  }
  return 1;
}

void print_percentage() {
  LCD_GoTo(LINE_START, SECOND_LINE);
  printf("       %d", (100 * progress) / MAX_PROGRESS);
  putchar(0b100101);
  LCD_GoTo(cursor_position, FIRST_LINE);
}

int main()
{
  /* Display initialization */
  LCD_Initialize();
  fdev_setup_stream(&hd44780_file, hd44780_transmit, NULL, _FDEV_SETUP_WRITE);
  stdout = stderr = &hd44780_file;

  initialize_progress_bar();
  progress = NO_PROGRESS;
  /* Main loop */
  while(1) {
    if(next_step()) {
      progress++;
      print_percentage();
    }
    _delay_ms(200);
  }
}

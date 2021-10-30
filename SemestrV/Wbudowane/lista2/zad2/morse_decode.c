#include <avr/io.h>
#include <stdio.h>
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

/*************************MORSE DECODER*************************/

#define BTN PC0
#define BTN_PIN PINC
#define BTN_PORT PORTC

#define DISPLAY_DDR DDRD
#define DISPLAY_PORT PORTD

#define QUANT 50
#define TOLERANCE 10
#define NUMBER_OF_LETTERS 26

typedef struct {
  uint8_t length;
  uint8_t code;
} character;

character LETTERS[26] ={
  {2, 0b00000001},//A
  {4, 0b00001110},//B
  {4, 0b00001010},//C
  {3, 0b00000110},//D
  {1, 0b00000001},//E
  {4, 0b00001011},//F
  {3, 0b00000100},//G
  {4, 0b00001111},//H
  {2, 0b00000011},//I
  {4, 0b00000001},//J
  {3, 0b00000010},//K
  {4, 0b00001101},//L
  {2, 0b00000000},//M
  {2, 0b00000010},//N
  {3, 0b00000000},//O
  {4, 0b00001001},//P
  {4, 0b00000100},//Q
  {3, 0b00000101},//R
  {3, 0b00000111},//S
  {1, 0b00000000},//T
  {3, 0b00000011},//U
  {4, 0b00000111},//V
  {3, 0b00000001},//W
  {4, 0b00000110},//X
  {4, 0b00000010},//Y
  {4, 0b00001100}//Z
};

typedef enum {
  IDLE,
  // UNCERTAIN_DOT,
  DOT,
  DAH,
  // UNCERTAIN_SPACE,
  S_SPACE,
  L_SPACE,
  W_SPACE,
} state;

void display_state(state state){
  DISPLAY_PORT = 1 << (state + 2) | 3;
}

void print_character_and_reset(character * character) {
  int8_t length = character->length;
  int8_t code = character->code;
  int8_t letter_printed = 0;

  for (int i = 0; i < NUMBER_OF_LETTERS; i++) {
    if((LETTERS[i].length == length) &&
        LETTERS[i].code == code ) {
      printf("%c", (char)(i + 65) );
      letter_printed = 1;
      break;
    }
  }

  if(!letter_printed) printf("?");
  character->length = 0;
  character->code = 0;
}

state handle_idle(int8_t next_signal) {
  if(!next_signal) return IDLE;
  return DOT;
}

state handle_dot(
  int8_t next_signal,
  uint16_t o_counter,
  character * character
) {
  if(!next_signal) {
    character->code |= 1 << (character->length);
    character->length += 1;
    return S_SPACE;
  }
  if( o_counter > QUANT ) return DAH;
  return DOT;
}

state handle_dah(
  int8_t next_signal,
  character * character
) {
  if(!next_signal) {
    character->length += 1;
    return S_SPACE;
  }
  return DAH;
}

state handle_s_space(
  int8_t next_signal,
  uint16_t z_counter,
  character * character
) {
  if(next_signal) return DOT;
  if(z_counter > QUANT) {
    print_character_and_reset(character);
    return L_SPACE;
  }
  return S_SPACE;
}

state handle_l_space(
  int8_t next_signal,
  uint16_t z_counter
) {
  if(next_signal) return DOT;
  if(z_counter > QUANT * 3) return W_SPACE;
  return L_SPACE;
}

state handle_w_space(
  int8_t next_signal,
  uint16_t z_counter
) {
  if(next_signal){
    printf(" ");
    return DOT;
  }
  if(z_counter > 7 * QUANT){
    printf(" ");
    return IDLE;
  }
  return W_SPACE;
}

void morse_decoder(int8_t next_sig) {
  static state state = IDLE; 
  static uint16_t z_time_counter = 0;
  static uint16_t o_time_counter = 0;
  static character character = {.length=0, .code=0};

  if(next_sig) {
    z_time_counter = 0;
    o_time_counter += 1;
  }
  else if(!(state == IDLE)) {
    o_time_counter = 0;
    z_time_counter += 1;
  }
  else {
    o_time_counter = 0;
    z_time_counter = 0;
  }

  switch (state)
  {
  case IDLE:
    state = handle_idle(next_sig);
    break;
  case DOT:
    state = handle_dot(next_sig, o_time_counter, &character);
    break;
  case DAH:
    state = handle_dah(next_sig, &character);
    break;
  case S_SPACE:
    state = handle_s_space(next_sig, z_time_counter, &character);
    break;
  case L_SPACE:
    state = handle_l_space(next_sig, z_time_counter);
    break;
  case W_SPACE:
    state = handle_w_space(next_sig, z_time_counter);
    break;    
  default:
    break;
  }

  display_state(state);
}

int main() {
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  // UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
  DISPLAY_DDR = 0xFC;
  DISPLAY_PORT = 3;
  BTN_PORT |= _BV(BTN);

  printf("Morse decoder\r\n");
  while (1) {
    morse_decoder(!(BTN_PIN & _BV(BTN)));
    _delay_ms(10);
  }
}

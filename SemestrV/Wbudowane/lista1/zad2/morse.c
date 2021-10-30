#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define SPACE 32
#define SC_A 97
#define SC_Z 122

#define QUANT 300

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

/****************************************************************/

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

void turn_on(){
  LED_PORT = _BV(LED);
}

void turn_off(){
  LED_PORT = ~_BV(LED);
}

void symbol_space(){
  turn_off();
  _delay_ms(QUANT);
}

void letter_space(){
  turn_off();
  _delay_ms(2*QUANT);
}

void word_space(){
  turn_off();
  _delay_ms(7*QUANT);
}

void dot() {
  turn_on();
  _delay_ms(QUANT);
}

void dah() {
  turn_on();
  _delay_ms(3 * QUANT);
}

void print_character(char a){
  uint8_t ascii_number = (uint8_t)a;

  if(ascii_number == SPACE){
    word_space();
    return;
  }

  if(ascii_number >= SC_A && ascii_number <= SC_Z) ascii_number -= 32; 

  uint8_t ascii_with_offset =  ascii_number - 65;
  character x = LETTERS[ascii_with_offset];
  uint8_t code_length = x.length;
  uint8_t code = x.code;

  for (int i = 0; i < code_length; i++)
  {
    if((code & 1) == 1) dot();
    else dah();
    symbol_space();
    code >>= 1;
  }

  letter_space();
}


int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  LED_DDR |= _BV(LED);

  printf("Enter sentance:\r\n");
  while(1) {
    char str[100];
    // scanf("%s", c);
    scanf(" %[^\r\n]s",str);
    printf("Odczytano: %s\r\n", str);

    for(int i=0; (i<100) & (str[i]!=0); i++){
      print_character(str[i]);
    }
  }
}

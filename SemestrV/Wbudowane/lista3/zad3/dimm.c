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

/**************************************************************/

#define LED PD6
#define LED_DDR DDRD
#define LED_PORT PORTD

const int16_t VALUES[256] = {
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  
  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  
  4,  4,  4,  4,  4,  4,  5,  5,  5,  5,  5,  5,  6,  6,  6,  6,  6,  7,  7,  7,  
  7,  8,  8,  8,  8,  9,  9,  9,  9,  10,  10,  10,  11,  11,  11,  12,  12,  
  12,  13,  13,  14,  14,  15,  15,  15,  16,  16,  17,  17,  18,  18,  19,  19,  
  20,  21,  21,  22,  23,  23,  24,  25,  25,  26,  27,  28,  28,  29,  30,  31,  
  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  45,  46,  47,  49,  
  50,  51,  53,  54,  56,  57,  59,  61,  63,  64,  66,  68,  70,  72,  74,  76,  
  78,  80,  82,  85,  87,  90,  92,  95,  97,  100,  103,  106,  109,  112,  
  115,  118,  121,  125,  128,  132,  136,  139,  143,  147,  151,  155,  160,  
  164,  169,  173,  178,  183,  188,  194,  199,  204,  210,  216,  222,  228,  
  234,  241,  248,  255,  262,  269,  276,  284,  292,  300,  308,  317,  325,  
  334,  344,  353,  363,  373,  383,  394,  405,  416,  428,  439,  452,  464,  
  477,  490,  504,  518,  532,  547,  562,  577,  593,  609,  626,  644,  661,  
  680,  698,  718,  737,  758,  779,  800,  822,  845,  868,  892,  917,  942,  
  968,  995,  1023};

void adc_init()
{
  ADMUX   = _BV(REFS0); // referencja AVcc, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADEN); // włącz ADC
}

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  
  LED_DDR = _BV(LED);
  adc_init();
  while(1) {
    ADCSRA |= _BV(ADSC); // wykonaj konwersję
    while (!(ADCSRA & _BV(ADIF))); // czekaj na wynik
    ADCSRA |= _BV(ADIF); // wyczyść bit ADIF (pisząc 1!)
    uint16_t v = ADC; // weź zmierzoną wartość (0..1023)
    uint8_t index = (uint8_t)(v >> 2);
    int us_spent_on = VALUES[index];

    for (int i = 0; i < 10; i++)
    {
      for (int i = 0; i < us_spent_on; i++)
      {
        LED_PORT = _BV(LED);
        _delay_us(1);
      }
      
      for (int i = 0; i < VALUES[255] - us_spent_on; i++)
      {
        LED_PORT = 0;
        _delay_us(1);
      }
    }
    
  }
}

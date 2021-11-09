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
  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  2,  2,  3,  3,  3,  
  4,  4,  5,  6,  6,  7,  8,  8,  9,  10,  11,  12,  12,  13,  14,  
  15,  16,  17,  18,  19,  21,  22,  23,  24,  25,  27,  28,  29,  
  31,  32,  33,  35,  36,  38,  40,  41,  43,  44,  46,  48,  49,  
  51,  53,  55,  57,  59,  61,  62,  64,  66,  69,  71,  73,  75,  
  77,  79,  81,  84,  86,  88,  91,  93,  95,  98,  100,  103,  105,  
  108,  111,  113,  116,  119,  121,  124,  127,  130,  133,  135,  
  138,  141,  144,  147,  150,  153,  156,  160,  163,  166,  169,  
  172,  176,  179,  182,  186,  189,  192,  196,  199,  203,  206,  
  210,  214,  217,  221,  225,  228,  232,  236,  240,  244,  248,  
  251,  255,  259,  263,  267,  272,  276,  280,  284,  288,  292,  
  297,  301,  305,  310,  314,  318,  323,  327,  332,  336,  341,  
  346,  350,  355,  360,  364,  369,  374,  379,  383,  388,  393,  
  398,  403,  408,  413,  418,  423,  428,  434,  439,  444,  449,  
  454,  460,  465,  470,  476,  481,  487,  492,  498,  503,  509,  
  515,  520,  526,  532,  537,  543,  549,  555,  561,  566,  572,  
  578,  584,  590,  596,  602,  609,  615,  621,  627,  633,  640,  
  646,  652,  658,  665,  671,  678,  684,  691,  697,  704,  710,  
  717,  724,  730,  737,  744,  751,  757,  764,  771,  778,  785,  
  792,  799,  806,  813,  820,  827,  834,  842,  849,  856,  863,  
  871,  878,  885,  893,  900,  908,  915,  923,  930,  938,  945,  
  953,  961,  968,  976,  984,  992,  1000};

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
      LED_PORT = _BV(LED);
      for (int i = 0; i < us_spent_on; i++)
      {
        _delay_us(1);
      }
      
      LED_PORT = 0;
      for (int i = 0; i < 1000 - us_spent_on; i++)
      {
        _delay_us(1);
      }
    }
    
  }
}

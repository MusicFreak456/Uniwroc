#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>

#define LED PB1
#define LED_DDR DDRB
#define LED_PORT PORTB

const int16_t VALUES[256] = {
  1011,  995,  968,  942,  917,  892,  868,  845,  822,  800,  779,  758,  737,  
  718,  698,  680,  661,  644,  626,  609,  593,  577,  562,  547,  532,  518,  
  504,  490,  477,  464,  452,  439,  428,  416,  405,  394,  383,  373,  363,  
  353,  344,  334,  325,  317,  308,  300,  292,  284,  276,  269,  262,  255,  
  248,  241,  234,  228,  222,  216,  210,  204,  199,  194,  188,  183,  178,  
  173,  169,  164,  160,  155,  151,  147,  143,  139,  136,  132,  128,  125,  
  121,  118,  115,  112,  109,  106,  103,  100,  97,  95,  92,  90,  87,  85,  
  82,  80,  78,  76,  74,  72,  70,  68,  66,  64,  63,  61,  59,  57,  56,  54,  
  53,  51,  50,  49,  47,  46,  45,  43,  42,  41,  40,  39,  38,  37,  36,  35,  
  34,  33,  32,  31,  30,  29,  28,  28,  27,  26,  25,  25,  24,  23,  23,  22,  
  21,  21,  20,  19,  19,  18,  18,  17,  17,  16,  16,  15,  15,  15,  14,  14,  
  13,  13,  12,  12,  12,  11,  11,  11,  10,  10,  10,  9,  9,  9,  9,  8,  8,  
  8,  8,  7,  7,  7,  7,  6,  6,  6,  6,  6,  5,  5,  5,  5,  5,  5,  4,  4,  4,  
  4,  4,  4,  3,  3,  3,  3,  3,  3,  3,  3,  3,  2,  2,  2,  2,  2,  2,  2,  2,  
  2,  2,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
  0,  0,  0};

void adc_init()
{
  ADMUX   = _BV(REFS0); // referencja AVcc, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADEN); // włącz ADC
}

void timer1_init()
{
  // ustaw tryb licznika
  // COM1A = 10   -- non-inverting mode
  // WGM1  = 1110 -- fast PWM top=ICR1
  // CS1   = 101  -- prescaler 1024
  // ICR1  = 1023
  // częstotliwość 16e6/(1024*(1+1023)) -- około 60 Hz
  // wzór: datasheet 20.12.3 str. 164
  ICR1 = 1023;
  TCCR1A = _BV(COM1A1) | _BV(WGM11);
  TCCR1B = _BV(WGM12) | _BV(WGM13) | _BV(CS12);
  // ustaw pin OC1A (PB1) jako wyjście
  DDRB |= _BV(PB1);
}

int main()
{
  LED_DDR = _BV(LED);
  adc_init();
  timer1_init();
  while(1) {
    ADCSRA |= _BV(ADSC); // wykonaj konwersję
    while (!(ADCSRA & _BV(ADIF))); // czekaj na wynik
    ADCSRA |= _BV(ADIF); // wyczyść bit ADIF (pisząc 1!)
    uint16_t v = ADC; // weź zmierzoną wartość (0..1023)
    uint8_t index = (uint8_t)(v >> 2);

    int cmp_value = VALUES[index];
    OCR1A = cmp_value;

    _delay_ms(10);
    
  }
}

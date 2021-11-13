#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>

#define RED_LED   _BV(PD5)
#define GREEN_LED _BV(PD3)
#define BLUE_LED  _BV(PD6)

#define LED_DDR  DDRD
#define LED_PORT PORTD

#define ANODE _BV(PB1)

#define NUMBER_DIM_DEGREES 256
#define TOP 255

const uint8_t DIMM_VALUES[NUMBER_DIM_DEGREES] = {
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  1,  
  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  
  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  
  4,  4,  4,  5,  5,  5,  5,  5,  5,  5,  6,  6,  6,  6,  6,  6,  7,  7,  7,  7,  
  7,  7,  8,  8,  8,  8,  9,  9,  9,  9,  9,  10,  10,  10,  10,  11,  11,   11,  
  12,  12,  12,  12,  13,  13,  13,  14,  14,  14,  15,  15,  15,  16,  16,  17,  
  17,  17,  18,  18,  19,  19,  19,  20,  20,  21,  21,  22,  22,  23,  23,  24,  
  25,  25,  26,  26,  27,  28,  28,  29,  30,  30,  31,  32,  32,  33,  34,  35,  
  35,  36,  37,  38,  39,  40,  41,  42,  42,  43,  44,  45,  46,  48,  49,  50,  
  51,  52,  53,  54,  56,  57,  58,  59,  61,  62,  64,  65,  66,  68,  69,  71,  
  73,  74,  76,  78,  79,  81,  83,  85,  87,  89,  91,  93,  95,  97,  99, 101,  
  103,  106,  108,  111,  113,  116,  118,  121,  123,  126,  129,  132,  135,  
  138,  141,  144,  147,  150,  154,  157,  161,  164,  168,  172,  175,  179,  
  183,  187,  191,  196,  200,  204,  209,  214,  218,  223,  228,  233,  238,  
  244,  249,  255};

const uint8_t SIN[NUMBER_DIM_DEGREES] = {
  0,  1,  3,  4,  6,  7,  9,  10,  12,  14,  15,  17,  18,  20,  21,  23,  25,  
  26,  28,  29,  31,  32,  34,  36,  37,  39,  40,  42,  43,  45,  46,  48,  49,  
  51,  53,  54,  56,  57,  59,  60,  62,  63,  65,  66,  68,  69,  71,  72,  74,  
  75,  77,  78,  80,  81,  83,  84,  86,  87,  89,  90,  92,  93,  95,  96,  97,  
  99,  100,  102,  103,  105,  106,  108,  109,  110,  112,  113,  115,  116,  
  117,  119,  120,  122,  123,  124,  126,  127,  128,  130,  131,  132,  134,  
  135,  136,  138,  139,  140,  142,  143,  144,  146,  147,  148,  149,  151,  
  152,  153,  154,  156,  157,  158,  159,  161,  162,  163,  164,  165,  167,  
  168,  169,  170,  171,  172,  174,  175,  176,  177,  178,  179,  180,  181,  
  183,  184,  185,  186,  187,  188,  189,  190,  191,  192,  193,  194,  195,  
  196,  197,  198,  199,  200,  201,  202,  203,  204,  205,  206,  207,  208,  
  209,  209,  210,  211,  212,  213,  214,  215,  215,  216,  217,  218,  219,  
  220,  220,  221,  222,  223,  223,  224,  225,  226,  226,  227,  228,  228,  
  229,  230,  230,  231,  232,  232,  233,  234,  234,  235,  236,  236,  237,  
  237,  238,  238,  239,  239,  240,  241,  241,  242,  242,  243,  243,  243,  
  244,  244,  245,  245,  246,  246,  246,  247,  247,  248,  248,  248,  249,  
  249,  249,  250,  250,  250,  250,  251,  251,  251,  251,  252,  252,  252,  
  252,  253,  253,  253,  253,  253,  253,  254,  254,  254,  254,  254,  254,  
  254,  254,  254,  254,  254,  254,  254,  254,  255};

/* http://www.retroprogramming.com/2017/07/xorshift-pseudorandom-numbers-in-z80.html */
uint16_t rand16(uint16_t seed){
  seed ^= seed << 7;
  seed ^= seed >> 9;
  seed ^= seed << 8;
  return seed;
}

void timer1_init()
{
  // ustaw tryb licznika
  // COM1A = 11   -- inverting mode
  // WGM1  = 1110 -- fast PWM top=ICR1
  // CS1   = 101  -- prescaler 1024
  // ICR1  = 1023
  ICR1 = TOP;
  TCCR1A = _BV(COM1A1) | _BV(COM1A0) | _BV(WGM11);
  TCCR1B = _BV(WGM12) | _BV(WGM13) | _BV(CS12) | _BV(CS10);
  DDRB |= ANODE;
}

void timer2_init(){
  // ustaw tryb licznika
  // COM0A = 10   -- non-inverting mode
  // COM0B = 10   -- non-inverting mode
  // WGM0  = 011  -- fast PWM top=0xFF
  // CS0   = 011  -- prescaler 64
  TCCR0A = _BV(COM0A1) | _BV(COM0B1) | _BV(WGM01) | _BV(WGM00);
  TCCR0B = _BV(CS01) | _BV(CS00);
  LED_DDR |= BLUE_LED | RED_LED;
}

void timer3_init(){
  // ustaw tryb licznika
  // COM2B = 10   -- non-inverting mode
  // WGM2  = 011  -- fast PWM top=0xFF
  // CS0   = 100  -- prescaler 64
  TCCR2A = _BV(COM2B1) | _BV(WGM21) | _BV(WGM20);
  TCCR2B = _BV(CS22);
  LED_DDR |= GREEN_LED;
}


void step(int x){
  OCR1A = TOP - DIMM_VALUES[SIN[x]];
  _delay_ms(6);
}

#define RED_INTENSITY   OCR0B
#define GREEN_INTENSITY OCR2B
#define BLUE_INTENSITY  OCR0A

uint16_t choose_random_color(uint16_t seed) {
  uint8_t rgb_values[3];
  uint8_t used[] = {0,0,0};

  uint16_t next_seed = rand16(seed);

  uint8_t choose_i_for_max = (uint8_t)next_seed % 3;
  rgb_values[choose_i_for_max] = 0;
  used[choose_i_for_max] = 1;

  next_seed = rand16(next_seed);
  uint8_t second = (uint8_t)next_seed & 1;

  for(int i = 0; i < 3; i++){
    if(!used[i]){
      if(second) second--;
      else {
        rgb_values[i] = 255;
        used[i] = 1;
        break;
      }
    }
  }

  next_seed = rand16(next_seed);
  uint8_t modulated_value = (uint8_t)next_seed;

  for(int i = 0; i < 3; i++){
    if(!used[i]) rgb_values[i] = modulated_value;
  }

  RED_INTENSITY = rgb_values[0];
  GREEN_INTENSITY = rgb_values[1];
  BLUE_INTENSITY = rgb_values[2];

  return next_seed;
}

int main()
{
  uint16_t seed = 42;

  RED_INTENSITY   = 255;
  GREEN_INTENSITY = 255;
  BLUE_INTENSITY  = 255;
  timer1_init();
  timer2_init();
  timer3_init();

  DDRB |= ANODE;
  PORTB = ANODE;
  while(1) {
    seed = choose_random_color(seed);
    for (int i = 0; i < NUMBER_DIM_DEGREES; i++)
    {
      step(i);
    }

    for (int i = NUMBER_DIM_DEGREES - 2; i >= 1; i--)
    {
      step(i);
    }
  }
}

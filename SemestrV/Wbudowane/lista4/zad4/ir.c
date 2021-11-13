#include <avr/io.h>
#include <util/delay.h>

#define OFF_CMP 421
#define ON_CMP  50

#define DETECTOR _BV(PB0)
#define DETECTOR_PIN PINB
#define DETECTOR_PORT PORTB

#define LED _BV(PB5)
#define LED_DDR DDRB
#define LED_PORT PORTB

void timer1_init()
{
  // ustaw tryb licznika
  // COM1A = 11   -- inverting mode
  // WGM1  = 1110 -- fast PWM top=ICR1
  // CS1   = 001  -- prescaler 1
  // ICR1  = 421
  // częstotliwość 16e6/(1*(1+321)) około 37.9 Hz
  ICR1 = 421;
  TCCR1A = _BV(COM1A1) | _BV(COM1A0) | _BV(WGM11);
  TCCR1B = _BV(WGM12) | _BV(WGM13) | _BV(CS10);
  DDRB |= _BV(PB1);
}

int main()
{

  OCR1A = OFF_CMP;
  timer1_init();

  LED_DDR |= LED;
  while(1) {
    for (int i = 0; i < 6; i++)
    {
      OCR1A = ON_CMP;
      _delay_us(550);
      if(DETECTOR_PIN & DETECTOR){
        LED_PORT &= ~LED;
      } else {
        LED_PORT = LED;
      }
      _delay_us(50);
      OCR1A = OFF_CMP;
      _delay_us(600);
    }
    _delay_us(100000 - 12 * 600);
    
  }
}

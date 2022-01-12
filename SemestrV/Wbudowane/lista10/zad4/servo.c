#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

#define TOP 1249
#define MAX_READING 1024.0
#define MAX_LEFT 143
#define MAX_RIGHT 32

void timer1_init() {
  // ustaw tryb licznika
  // COM1A = 10   -- inverting mode
  // WGM1  = 1110 -- fast PWM top=ICR1
  // CS1   = 100  -- prescaler 256
  // ICR1  = 1249
  // częstotliwość 16e6/(256*(1+1249)) = 50Hz
  ICR1 = TOP;
  TCCR1A  = _BV(COM1B1) | _BV(WGM11);
  TCCR1B  = _BV(WGM12) | _BV(WGM13) | _BV(CS12);
  // ustaw pin OC1B (PB2) jako wyjście
  OCR1B = 82;
  DDRB |= _BV(PB2);
}

void adc_init() {
  ADMUX   = _BV(REFS0); // referencja Vin, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADIE);  // włącz przerwania ADC
  ADCSRA |= _BV(ADATE); // włącz auto-trigger
  ADCSRB  = _BV(ADTS2); // konwersja przy overflow w liczniku 0
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

void timer0_init() {
  // Preskaler: 1024
  // Top: 0xFF
  // Częstotliwość: około 61 Hz
  TCCR0B = _BV(CS02) | _BV(CS00);
  TIMSK0 |= _BV(TOIE0);
}

uint16_t map(uint16_t adc) {
  float frac = adc / MAX_READING;
  return frac * (MAX_LEFT - MAX_RIGHT) + MAX_RIGHT;
}

ISR(ADC_vect) {
  OCR1B = map(ADC);
}

ISR(TIMER0_OVF_vect) {
}

int main() {
  timer1_init();

  adc_init();
  timer0_init();

  set_sleep_mode(SLEEP_MODE_IDLE);
  sei();

  while (1) {
    sleep_mode();
  }
}
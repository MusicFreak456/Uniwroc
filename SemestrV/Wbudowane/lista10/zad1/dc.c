#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <stdio.h>

void timer1_init() {
  // ustaw tryb licznika
  // COM1A = 10   -- non-inverting mode
  // WGM1  = 1110 -- fast PWM top=ICR1
  // CS1   = 010  -- prescaler 8
  // ICR1  = 2048
  // częstotliwość 16e6/(8*(1+2048)) = około 976 Hz
  ICR1 = 2048;
  TCCR1A = _BV(COM1A1) | _BV(WGM11);
  TCCR1B = _BV(WGM12) | _BV(WGM13) | _BV(CS11);
  // ustaw pin OC1A (PB1) jako wyjście
  DDRB |= _BV(PB1);
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

ISR(ADC_vect) {
  OCR1A = ADC * 2;
}

ISR(TIMER0_OVF_vect) {
}

int main() {
  timer1_init();
  OCR1A = ICR1/2;

  adc_init();
  timer0_init();

  set_sleep_mode(SLEEP_MODE_IDLE);
  sei();

  while(1) {
    sleep_mode();
  }
}

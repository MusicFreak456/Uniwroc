#include <avr/io.h>
#include <avr/pgmspace.h>
#include <util/delay.h>

#define BUZZ PD3
#define BUZZ_DDR DDRD
#define BUZZ_PORT PORTD    

#define US_IN_S 1000000

#define TEMPO_BPM 112 // uderzenia na minutę
// sygnatura to 4/4 więc cała nuta będzie odpowiedać 4 uderzeniom
#define WHOLE_NOTE_DURATION ((60000 * 4) / TEMPO_BPM)  //ms

#define A4_FREQ 440
#define B4_FREQ 494
#define D5_FREQ 587
#define CS5_FREQ 554
#define E5_FREQ 659
#define FS5_FREQ 740
#define A5_FREQ 880

#define TONE(step, duration) \
  for (uint16_t i = 0; i < (uint32_t)1000 * (duration * 0.9) / (step); i++) { \
    BUZZ_PORT |= _BV(BUZZ); \
    _delay_us(step / 2); \
    BUZZ_PORT &= ~_BV(BUZZ); \
    _delay_us(step / 2); \
  } \
  _delay_ms(duration * 0.1)

#define A4_TONE(duration) TONE(US_IN_S/A4_FREQ, duration)
#define B4_TONE(duration) TONE(US_IN_S/B4_FREQ, duration)
#define D5_TONE(duration) TONE(US_IN_S/D5_FREQ, duration)
#define CS5_TONE(duration) TONE(US_IN_S/CS5_FREQ, duration)
#define E5_TONE(duration) TONE(US_IN_S/E5_FREQ, duration)
#define FS5_TONE(duration) TONE(US_IN_S/FS5_FREQ, duration)
#define A5_TONE(duration) TONE(US_IN_S/A5_FREQ, duration)
#define NO_TONE_TONE(duration) {}

#define PLAY_WHOLENOTE(tone_macro)  tone_macro(WHOLE_NOTE_DURATION)
#define PLAY_HALFNOTE(tone_macro) tone_macro(WHOLE_NOTE_DURATION/2)
#define PLAY_QUARTERNOTE(tone_macro) tone_macro(WHOLE_NOTE_DURATION/4)
#define PLAY_QUARTERNOTE_DOT(tone_macro) tone_macro((uint16_t)(((WHOLE_NOTE_DURATION/4)*1.5)))
#define PLAY_EIGHTHNOTE(tone_macro) tone_macro(WHOLE_NOTE_DURATION/8)
#define PLAY_EIGHTHNOTE_DOT(tone_macro) tone_macro((uint16_t)(((WHOLE_NOTE_DURATION/8)*1.5)))
#define PLAY_SIXTHNOTE(tone_macro) tone_macro(WHOLE_NOTE_DURATION/16)
#define PLAY_HALFREST _delay_ms(WHOLE_NOTE_DURATION/2)
#define PLAY_EIGHTHREST _delay_ms(WHOLE_NOTE_DURATION/8)
#define PLAY_WHOLEREST _delay_ms(WHOLE_NOTE_DURATION)

typedef enum {
  A4,
  B4,
  D5,
  CS5,
  E5,
  FS5,
  A5,
  NO_TONE
} tone;

typedef enum {
  WHOLENOTE,
  HALFNOTE,
  QUARTERNOTE,
  QUARTERNOTE_DOT,
  EIGHTHNOTE,
  EIGHTHNOTE_DOT,
  SIXTHNOTE,
  HALFREST,
  EIGHTHREST,
  WHOLEREST
} duration;

typedef struct {
  tone tone;
  duration duration;
} note;

const uint16_t SONG_LENGTH = 77;

const note RICKROLL[] PROGMEM = {
  {.tone = D5, .duration=HALFNOTE},
  {.tone = E5, .duration=EIGHTHNOTE},
  {.tone = FS5, .duration=EIGHTHNOTE},
  {.tone = D5, .duration=EIGHTHNOTE},
  {.tone = E5, .duration=EIGHTHNOTE},
  {.tone = E5, .duration=EIGHTHNOTE},
  {.tone = E5, .duration=EIGHTHNOTE},
  {.tone = FS5, .duration=EIGHTHNOTE},
  {.tone = E5, .duration=QUARTERNOTE},
  {.tone = A4, .duration=QUARTERNOTE},
  {.tone = NO_TONE, .duration=HALFREST},

  {.tone = B4, .duration=EIGHTHNOTE},
  {.tone = CS5, .duration=EIGHTHNOTE},
  {.tone = D5, .duration=EIGHTHNOTE},
  {.tone = B4, .duration=EIGHTHNOTE},
  {.tone = NO_TONE, .duration=EIGHTHREST},

  {.tone = E5, .duration=EIGHTHNOTE},
  {.tone = FS5, .duration=EIGHTHNOTE},
  {.tone = E5, .duration=EIGHTHNOTE_DOT},

  {.tone = A4, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = FS5, .duration=EIGHTHNOTE_DOT},
  {.tone = FS5, .duration=EIGHTHNOTE_DOT},
  {.tone = E5, .duration=QUARTERNOTE_DOT},

  {.tone = A4, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = E5, .duration=EIGHTHNOTE_DOT},
  {.tone = E5, .duration=EIGHTHNOTE_DOT},
  {.tone = D5, .duration=EIGHTHNOTE_DOT},
  {.tone = CS5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=EIGHTHNOTE_DOT},

  {.tone = A4, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=QUARTERNOTE},
  {.tone = E5, .duration=EIGHTHNOTE},
  {.tone = CS5, .duration=EIGHTHNOTE_DOT},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = A4, .duration=EIGHTHNOTE},
  {.tone = A4, .duration=EIGHTHNOTE},
  {.tone = A4, .duration=EIGHTHNOTE},
  {.tone = E5, .duration=QUARTERNOTE},
  {.tone = D5, .duration=HALFNOTE},

  {.tone = A4, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = FS5, .duration=EIGHTHNOTE_DOT},
  {.tone = FS5, .duration=EIGHTHNOTE_DOT},
  {.tone = E5, .duration=QUARTERNOTE_DOT},

  {.tone = A4, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = A5, .duration=QUARTERNOTE},
  {.tone = CS5, .duration=EIGHTHNOTE},
  {.tone = D5, .duration=EIGHTHNOTE_DOT},
  {.tone = CS5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=EIGHTHNOTE},

  {.tone = A4, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=SIXTHNOTE},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = D5, .duration=QUARTERNOTE},
  {.tone = E5, .duration=EIGHTHNOTE},
  {.tone = CS5, .duration=EIGHTHNOTE_DOT},
  {.tone = B4, .duration=SIXTHNOTE},
  {.tone = A4, .duration=QUARTERNOTE},
  {.tone = A4, .duration=EIGHTHNOTE},
  {.tone = E5, .duration=QUARTERNOTE},
  {.tone = D5, .duration=HALFNOTE},
  {.tone = NO_TONE, .duration=WHOLEREST}
};

#define DURATION_CASE_NOTE(duration_name, note_name) \
  case duration_name: \
    PLAY_ ## duration_name (note_name ## _TONE); \
    break;

#define DURATION_CASE_REST(rest_name) \
  case rest_name: \
      PLAY_ ## rest_name; \
      break;

#define DURATION_SWITCH(note_name) \
  switch (curr_duration) \
  { \
  DURATION_CASE_NOTE(WHOLENOTE,note_name) \
  DURATION_CASE_NOTE(HALFNOTE,note_name) \
  DURATION_CASE_NOTE(QUARTERNOTE,note_name) \
  DURATION_CASE_NOTE(QUARTERNOTE_DOT,note_name) \
  DURATION_CASE_NOTE(EIGHTHNOTE,note_name) \
  DURATION_CASE_NOTE(EIGHTHNOTE_DOT,note_name) \
  DURATION_CASE_NOTE(SIXTHNOTE,note_name) \
  DURATION_CASE_REST(WHOLEREST) \
  DURATION_CASE_REST(HALFREST) \
  DURATION_CASE_REST(EIGHTHREST) \
  default: \
    break; \
  }

#define TONE_CASE(note_name) \
  case note_name : \
    DURATION_SWITCH(note_name) \
    break;


int main() {
  BUZZ_DDR |= _BV(BUZZ);
  while (1) {
    for (int i = 0; i < SONG_LENGTH; i++)
    {
      tone curr_tone = pgm_read_byte(&(RICKROLL[i].tone));
      duration curr_duration = pgm_read_byte(&(RICKROLL[i].duration));

      switch (curr_tone)
      { 
      TONE_CASE(A4)
      TONE_CASE(B4)
      TONE_CASE(D5)
      TONE_CASE(CS5)
      TONE_CASE(E5)
      TONE_CASE(FS5)
      TONE_CASE(A5)
      TONE_CASE(NO_TONE)
      default:
        break;
      }
    }
  }
}
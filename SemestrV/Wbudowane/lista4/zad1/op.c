#include <avr/io.h>
#include <inttypes.h>

/******************************8 bitów******************************/

int8_t int8_t_add(int8_t a, int8_t b, uint16_t *start){
  *start = TCNT1;
  return a + b;
}

int8_t int8_t_sub(int8_t a, int8_t b, uint16_t *start){
  *start = TCNT1;
  return a - b;
}

int8_t int8_t_mul(int8_t a, int8_t b, uint16_t *start){
  *start = TCNT1;
  return a * b;
}

int8_t int8_t_div(int8_t a, int8_t b, uint16_t *start){
  *start = TCNT1;
  return a / b;
}

/******************************16 bitów******************************/

int16_t int16_t_add(int16_t a, int16_t b, uint16_t *start){
  *start = TCNT1;
  return a + b;
}

int16_t int16_t_sub(int16_t a, int16_t b, uint16_t *start){
  *start = TCNT1;
  return a - b;
}

int16_t int16_t_mul(int16_t a, int16_t b, uint16_t *start){
  *start = TCNT1;
  return a * b;
}

int16_t int16_t_div(int16_t a, int16_t b, uint16_t *start){
  *start = TCNT1;
  return a / b;
}

/******************************32 bity******************************/

int32_t int32_t_add(int32_t a, int32_t b, uint16_t *start){
  *start = TCNT1;
  return a + b;
}

int32_t int32_t_sub(int32_t a, int32_t b, uint16_t *start){
  *start = TCNT1;
  return a - b;
}

int32_t int32_t_mul(int32_t a, int32_t b, uint16_t *start){
  *start = TCNT1;
  return a * b;
}

int32_t int32_t_div(int32_t a, int32_t b, uint16_t *start){
  *start = TCNT1;
  return a / b;
}

/******************************64 bity******************************/

int64_t int64_t_add(int64_t a, int64_t b, uint16_t *start){
  *start = TCNT1;
  return a + b;
}

int64_t int64_t_sub(int64_t a, int64_t b, uint16_t *start){
  *start = TCNT1;
  return a - b;
}

int64_t int64_t_mul(int64_t a, int64_t b, uint16_t *start){
  *start = TCNT1;
  return a * b;
}

int64_t int64_t_div(int64_t a, int64_t b, uint16_t *start){
  *start = TCNT1;
  return a / b;
}

/******************************Float******************************/

float float_add(float a, float b, uint16_t *start){
  *start = TCNT1;
  return a + b;
}

float float_sub(float a, float b, uint16_t *start){
  *start = TCNT1;
  return a - b;
}

float float_mul(float a, float b, uint16_t *start){
  *start = TCNT1;
  return a * b;
}

float float_div(float a, float b, uint16_t *start){
  *start = TCNT1;
  return a / b;
}
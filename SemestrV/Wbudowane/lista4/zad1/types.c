#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>

#define LDS_COST 2*2
#define MOVW_COST 1
#define STD_COST 2
#define ST_COST 2
#define RETURN_COST 4

void print_cycles(uint16_t start, uint16_t end) {
  uint16_t diff;
  if(end > start) diff = end - start;
  else diff = (0xFFFF - start) + end; 
  diff -= LDS_COST + MOVW_COST + STD_COST + ST_COST + RETURN_COST;
  printf("Number of cycles: %"PRIu16"\r\n", diff);
}

int8_t int8_t_add(int8_t a, int8_t b, uint16_t *start);
int8_t int8_t_sub(int8_t a, int8_t b, uint16_t *start);
int8_t int8_t_mul(int8_t a, int8_t b, uint16_t *start);
int8_t int8_t_div(int8_t a, int8_t b, uint16_t *start);

void int8_t_calc(int8_t a, int8_t b) {
  printf("int8:\r\n");

  uint16_t start;

  int8_t res = int8_t_add(a,b,&start);
  uint16_t end = TCNT1;
  printf("+: %"PRId8"\r\n", res );
  print_cycles(start, end);

  res = int8_t_sub(a,b,&start);
  end = TCNT1;
  printf("-: %"PRId8"\r\n", res );
  print_cycles(start, end);

  res = int8_t_mul(a,b,&start);
  end = TCNT1;
  printf("*: %"PRId8"\r\n", res );
  print_cycles(start, end);

  res = int8_t_div(a,b,&start);
  end = TCNT1;
  printf("/: %"PRId8"\r\n", res );
  print_cycles(start, end);
}

int16_t int16_t_add(int16_t a, int16_t b, uint16_t *start);
int16_t int16_t_sub(int16_t a, int16_t b, uint16_t *start);
int16_t int16_t_mul(int16_t a, int16_t b, uint16_t *start);
int16_t int16_t_div(int16_t a, int16_t b, uint16_t *start);

void int16_t_calc(int16_t a, int16_t b) {
  printf("\nint16:\r\n");

  uint16_t start;

  int16_t res = int16_t_add(a,b,&start);
  uint16_t end = TCNT1;
  printf("+: %"PRId16"\r\n", res );
  print_cycles(start, end);

  res = int16_t_sub(a,b,&start);
  end = TCNT1;
  printf("-: %"PRId16"\r\n", res );
  print_cycles(start, end);

  res = int16_t_mul(a,b,&start);
  end = TCNT1;
  printf("*: %"PRId16"\r\n", res );
  print_cycles(start, end);

  res = int16_t_div(a,b,&start);
  end = TCNT1;
  printf("/: %"PRId16"\r\n", res );
  print_cycles(start, end);
}

int32_t int32_t_add(int32_t a, int32_t b, uint16_t *start);
int32_t int32_t_sub(int32_t a, int32_t b, uint16_t *start);
int32_t int32_t_mul(int32_t a, int32_t b, uint16_t *start);
int32_t int32_t_div(int32_t a, int32_t b, uint16_t *start);

void int32_t_calc(int32_t a, int32_t b) {
  printf("\nint32:\r\n");

  uint16_t start;

  int32_t res = int32_t_add(a,b,&start);
  uint16_t end = TCNT1;
  printf("+: %"PRId32"\r\n", res );
  print_cycles(start, end);

  res = int32_t_sub(a,b,&start);
  end = TCNT1;
  printf("-: %"PRId32"\r\n", res );
  print_cycles(start, end);

  res = int32_t_mul(a,b,&start);
  end = TCNT1;
  printf("*: %"PRId32"\r\n", res );
  print_cycles(start, end);

  res = int32_t_div(a,b,&start);
  end = TCNT1;
  printf("/: %"PRId32"\r\n", res );
  print_cycles(start, end);
}

int64_t int64_t_add(int64_t a, int64_t b, uint16_t *start);
int64_t int64_t_sub(int64_t a, int64_t b, uint16_t *start);
int64_t int64_t_mul(int64_t a, int64_t b, uint16_t *start);
int64_t int64_t_div(int64_t a, int64_t b, uint16_t *start);

void int64_t_calc(int64_t a, int64_t b) {
  printf("\nint64:\r\n");

  uint16_t start;

  int64_t res = int64_t_add(a,b,&start);
  uint16_t end = TCNT1;
  printf("+: %"PRId32"\r\n", (int32_t)res );
  print_cycles(start, end);

  res = int64_t_sub(a,b,&start);
  end = TCNT1;
  printf("-: %"PRId32"\r\n", (int32_t)res );
  print_cycles(start, end);

  res = int64_t_mul(a,b,&start);
  end = TCNT1;
  printf("*: %"PRId32"\r\n", (int32_t)res );
  print_cycles(start, end);

  res = int64_t_div(a,b,&start);
  end = TCNT1;
  printf("/: %"PRId32"\r\n", (int32_t)res );
  print_cycles(start, end);
}

float float_add(float a, float b, uint16_t *start);
float float_sub(float a, float b, uint16_t *start);
float float_mul(float a, float b, uint16_t *start);
float float_div(float a, float b, uint16_t *start);

void float_calc(float a, float b) {
  printf("\nfloat:\r\n");

  uint16_t start;

  float res = float_add(a,b,&start);
  uint16_t end = TCNT1;
  printf("+: %f\r\n", res );
  print_cycles(start, end);

  res = float_sub(a,b,&start);
  end = TCNT1;
  printf("-: %f\r\n", res );
  print_cycles(start, end);

  res = float_mul(a,b,&start);
  end = TCNT1;
  printf("*: %f\r\n", res );
  print_cycles(start, end);

  res = float_div(a,b,&start);
  end = TCNT1;
  printf("/: %f\r\n", res );
  print_cycles(start, end);
}

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

void timer1_init()
{
  // ustaw tryb licznika
  // WGM1  = 0000 -- normal
  // CS1   = 001  -- prescaler 1
  TCCR1B = _BV(CS10);
}

/**************************************************************/

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  timer1_init();
  while (1) {
    uint32_t a,b;
    printf("Enter two numbers separated by space:\r\n");
    scanf("%"SCNd32" %"SCNd32, &a, &b);
    printf("Odczytano: %"PRId32" %"PRId32"\r\n",a,b);
    int8_t_calc((int8_t)a, (int8_t)b);
    int16_t_calc((int16_t)a, (int16_t)b);
    int32_t_calc((int32_t)a, (int32_t)b);
    int64_t_calc((int64_t)a, (int64_t)b);
    float_calc((float)a, (float)b);
  }

  return 0;
}

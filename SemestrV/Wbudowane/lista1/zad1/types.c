#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>

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

/******************************8 bitów******************************/

int8_t int8_t_add(int8_t a, int8_t b){
  return a + b;
}

int8_t int8_t_sub(int8_t a, int8_t b){
  return a - b;
}

int8_t int8_t_mul(int8_t a, int8_t b){
  return a * b;
}

int8_t int8_t_div(int8_t a, int8_t b){
  return a / b;
}

void int8_t_calc(int8_t a, int8_t b) {
  printf("uint8:\r\n");
  printf("+: %"PRId8"\r\n", int8_t_add(a,b) );
  printf("-: %"PRId8"\r\n", int8_t_sub(a,b) );
  printf("*: %"PRId8"\r\n", int8_t_mul(a,b) );
  printf("/: %"PRId8"\r\n", int8_t_div(a,b) );
}

/******************************16 bitów******************************/

int16_t int16_t_add(int16_t a, int16_t b){
  return a + b;
}

int16_t int16_t_sub(int16_t a, int16_t b){
  return a - b;
}

int16_t int16_t_mul(int16_t a, int16_t b){
  return a * b;
}

int16_t int16_t_div(int16_t a, int16_t b){
  return a / b;
}

void int16_t_calc(int16_t a, int16_t b) {
  printf("uint16:\r\n");
  printf("+: %"PRId16"\r\n", int16_t_add(a,b) );
  printf("-: %"PRId16"\r\n", int16_t_sub(a,b) );
  printf("*: %"PRId16"\r\n", int16_t_mul(a,b) );
  printf("/: %"PRId16"\r\n", int16_t_div(a,b) );
}

/******************************32 bity******************************/

int32_t int32_t_add(int32_t a, int32_t b){
  return a + b;
}

int32_t int32_t_sub(int32_t a, int32_t b){
  return a - b;
}

int32_t int32_t_mul(int32_t a, int32_t b){
  return a * b;
}

int32_t int32_t_div(int32_t a, int32_t b){
  return a / b;
}

void int32_t_calc(int32_t a, int32_t b) {
  printf("uint32:\r\n");
  printf("+: %"PRId32"\r\n", int32_t_add(a,b) );
  printf("-: %"PRId32"\r\n", int32_t_sub(a,b) );
  printf("*: %"PRId32"\r\n", int32_t_mul(a,b) );
  printf("/: %"PRId32"\r\n", int32_t_div(a,b) );
}

/******************************64 bity******************************/

int64_t int64_t_add(int64_t a, int64_t b){
  return a + b;
}

int64_t int64_t_sub(int64_t a, int64_t b){
  return a - b;
}

int64_t int64_t_mul(int64_t a, int64_t b){
  return a * b;
}

int64_t int64_t_div(int64_t a, int64_t b){
  return a / b;
}

void int64_t_calc(int64_t a, int64_t b) {
  printf("uint64:\r\n");
  printf("+: %"PRId32"\r\n", (int32_t)int64_t_add(a,b) );
  printf("-: %"PRId32"\r\n", (int32_t)int64_t_sub(a,b) );
  printf("*: %"PRId32"\r\n", (int32_t)int64_t_mul(a,b) );
  printf("/: %"PRId32"\r\n", (int32_t)int64_t_div(a,b) );
}

/******************************Float******************************/

float float_add(float a, float b){
  return a + b;
}

float float_sub(float a, float b){
  return a - b;
}

float float_mul(float a, float b){
  return a * b;
}

float float_div(float a, float b){
  return a / b;
}

void float_calc(float a, float b) {
  printf("float:\r\n");
  printf("+: %f\r\n", float_add(a,b) );
  printf("-: %f\r\n", float_sub(a,b) );
  printf("*: %f\r\n", float_mul(a,b) );
  printf("/: %f\r\n", float_div(a,b) );
}

int main()
{
  uart_init();
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;

  while (1)
  {
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

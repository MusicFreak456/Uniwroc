/******************************************************************************
 * Header file inclusions.
 ******************************************************************************/

#include "FreeRTOS.h"
#include "task.h"
#include "semphr.h"
#include "queue.h"

#include <avr/io.h>
#include <stdio.h>
#include <avr/interrupt.h>
#include "uart.h"

/******************************************************************************
 * Idle task memory initialization
 ******************************************************************************/

#define mainIDLE_TASK_STACK_SIZE  ( ( unsigned short ) 150 )

/* configSUPPORT_STATIC_ALLOCATION is set to 1, so the application must provide an
implementation of vApplicationGetIdleTaskMemory() to provide the memory that is
used by the Idle task. */
void vApplicationGetIdleTaskMemory( StaticTask_t **ppxIdleTaskTCBBuffer,
                                    StackType_t **ppxIdleTaskStackBuffer,
                                    uint32_t *pulIdleTaskStackSize )
{
/* If the buffers to be provided to the Idle task are declared inside this
function then they must be declared static - otherwise they will be allocated on
the stack and so not exists after this function exits. */
static StaticTask_t xIdleTaskTCB;
static StackType_t uxIdleTaskStack[ mainIDLE_TASK_STACK_SIZE ];

    /* Pass out a pointer to the StaticTask_t structure in which the Idle task's
    state will be stored. */
    *ppxIdleTaskTCBBuffer = &xIdleTaskTCB;

    /* Pass out the array that will be used as the Idle task's stack. */
    *ppxIdleTaskStackBuffer = uxIdleTaskStack;

    /* Pass out the size of the array pointed to by *ppxIdleTaskStackBuffer.
    Note that, as the array is necessarily of type StackType_t,
    configMINIMAL_STACK_SIZE is specified in words, not bytes. */
    *pulIdleTaskStackSize = mainIDLE_TASK_STACK_SIZE;
}

/******************************************************************************
 * Private macro definitions.
 ******************************************************************************/

#define mainADC_TASK_PRIORITY   1

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vAdcCode(void* pvParameters);

/******************************************************************************
 * Global structures and handles
 ******************************************************************************/

StaticTask_t xAdc0Buffer;
StackType_t xAdc0Stack[ configMINIMAL_STACK_SIZE ];

StaticTask_t xAdc1Buffer;
StackType_t xAdc1Stack[ configMINIMAL_STACK_SIZE ];

StaticTask_t xAdc2Buffer;
StackType_t xAdc2Stack[ configMINIMAL_STACK_SIZE ];

SemaphoreHandle_t xPrintLock;
StaticSemaphore_t xPrintLockBuffer;

/******************************************************************************
 * Safe ADC implementation
 ******************************************************************************/

SemaphoreHandle_t xAdcLock;
StaticSemaphore_t xAdcLockBuffer;

SemaphoreHandle_t xAdcResLock;
StaticSemaphore_t xAdcResLockBuffer;

void adc_init() {
  xAdcLock = xSemaphoreCreateMutexStatic( &xAdcLockBuffer );
  xAdcResLock = xSemaphoreCreateBinaryStatic( &xAdcResLockBuffer );
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  ADCSRA |= _BV(ADIE);                            // włącz przerwania ADC
}

void adc_up(uint8_t mux) {
  ADMUX   = _BV(REFS0) | (mux & 0xF); // referencja AVcc, wejście mux
  ADCSRA |= _BV(ADEN);  // włącz ADC 
}

void adc_down() {
  ADCSRA &= ~_BV(ADEN);  // wyłącz ADC 
}

volatile uint16_t recorded_value;

ISR(ADC_vect) {
  recorded_value = ADC;
  xSemaphoreGiveFromISR(xAdcResLock, NULL);
}

uint16_t readADC(uint8_t mux) {
  xSemaphoreTake(xAdcLock, portMAX_DELAY);
  // printf("ADC lock acquired on ADC%d\r\n", mux);
  adc_up(mux);

  ADCSRA |= _BV(ADSC);

  // block until result received
  xSemaphoreTake(xAdcResLock, portMAX_DELAY);
  uint16_t res = recorded_value;

  adc_down();
  // printf("Returning lock\r\n");
  xSemaphoreGive(xAdcLock);

  return res;
}

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

int main(void)
{
  uart_init();
  stdin = stdout = stderr = &uart_file;
  adc_init();

  sei();

  xPrintLock = xSemaphoreCreateMutexStatic( &xPrintLockBuffer );

  // Create tasks.
  xTaskHandle adc0_handle;
  uint8_t mux0 = 0;
  adc0_handle = xTaskCreateStatic(
    vAdcCode,
    "adc0",
    configMINIMAL_STACK_SIZE,
    &mux0,
    mainADC_TASK_PRIORITY,
    xAdc0Stack,
    &xAdc0Buffer
  );

  xTaskHandle adc1_handle;
  uint8_t mux1 = 1;
  adc1_handle = xTaskCreateStatic(
    vAdcCode,
    "adc1",
    configMINIMAL_STACK_SIZE,
    &mux1,
    mainADC_TASK_PRIORITY,
    xAdc1Stack,
    &xAdc1Buffer
  );

  xTaskHandle adc2_handle;
  uint8_t mux2 = 2;
  adc2_handle = xTaskCreateStatic(
    vAdcCode,
    "adc2",
    configMINIMAL_STACK_SIZE,
    &mux2,
    mainADC_TASK_PRIORITY,
    xAdc2Stack,
    &xAdc2Buffer
  );

  // Start scheduler.
  vTaskStartScheduler();

  return 0;
}

/**************************************************************************//**
 * \fn static vApplicationIdleHook(void)
 *
 * \brief
 ******************************************************************************/
void vApplicationIdleHook(void) {

}

/******************************************************************************
 * Private function definitions.
 ******************************************************************************/

static void vAdcCode(void* pvParameters){
  while (1) {
    uint8_t mux = *(uint8_t *)pvParameters;
    uint16_t received = readADC(mux);

    xSemaphoreTake(xPrintLock, portMAX_DELAY);
    printf("ADC%"PRIu8": %"PRIu16"\r\n", mux, received);
    xSemaphoreGive(xPrintLock);
  }
}
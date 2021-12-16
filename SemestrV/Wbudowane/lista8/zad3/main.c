/******************************************************************************
 * Header file inclusions.
 ******************************************************************************/

#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"

#include <avr/io.h>
#include <avr/interrupt.h>
#include <stdio.h>
#include "uart.h"

/******************************************************************************
 * Idle task memory initialization
 ******************************************************************************/

#define mainIDLE_TASK_STACK_SIZE  ( ( unsigned short ) 200 )

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

#define mainBlink_TASK_PRIORITY   1
#define mainUart_TASK_PRIORITY    2

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vUartCode(void* pvParameters);
static void vBlinkCode(void* pvParameters);

/******************************************************************************
 * Global structures and handles
 ******************************************************************************/

StaticTask_t xBlinkBuffer;
StackType_t xBlinkStack[ configMINIMAL_STACK_SIZE ];

StaticTask_t xUartBuffer;
StackType_t xUartStack[ configMINIMAL_STACK_SIZE ];

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

int main(void)
{
  uart_init();
  stdin = stdout = stderr = &uart_file;

  sei();
  
  // Create tasks.
  xTaskHandle blink_handle;
  blink_handle = xTaskCreateStatic(
    vBlinkCode,
    "blink",
    configMINIMAL_STACK_SIZE,
    NULL,
    mainBlink_TASK_PRIORITY,
    xBlinkStack,
    &xBlinkBuffer
  );

  xTaskHandle uart_handle;
  uart_handle = xTaskCreateStatic(
    vUartCode,
    "uart",
    configMINIMAL_STACK_SIZE,
    NULL,
    mainUart_TASK_PRIORITY,
    xUartStack,
    &xUartBuffer
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

static void vUartCode(void* pvParameters) {
  while (1) {
    int input;
    printf("Enter number: ");
    scanf("%d", &input);
    printf("You entered: %d\r\n", input);
    // vTaskDelay(1000 / portTICK_PERIOD_MS);
  }
}

static void vBlinkCode(void* pvParameters){
  DDRB |= _BV(PB5);

  while (1) {
    PORTB ^= _BV(PB5);
    vTaskDelay(500 / portTICK_PERIOD_MS);
  }
}
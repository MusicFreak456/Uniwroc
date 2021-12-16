/******************************************************************************
 * Header file inclusions.
 ******************************************************************************/

#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"

#include <avr/io.h>
#include <stdio.h>
#include "uart.h"

/******************************************************************************
 * Idle task memory initialization
 ******************************************************************************/

#define mainIDLE_TASK_STACK_SIZE  ( ( unsigned short ) 85 )

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

#define mainLED_TASK_PRIORITY   2
#define mainREAD_TASK_PRIORITY  1


#define mainLED      PC0
#define mainLED_DDR  DDRC
#define mainLED_PORT PORTC

#define mainQUEUE_LENGTH 5
#define mainQUEUE_ITEM_SIZE sizeof( uint16_t )

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vReadCode(void* pvParameters);
static void vLedCode(void* pvParameters);

/******************************************************************************
 * Global structures and handles
 ******************************************************************************/

StaticTask_t xLedBuffer;
StackType_t xLedStack[ configMINIMAL_STACK_SIZE ];

StaticTask_t xReadBuffer;
StackType_t xReadStack[ configMINIMAL_STACK_SIZE ];

static StaticQueue_t xStaticReadQueue;
static QueueHandle_t xReadQueue;
uint8_t ucReadQueueStorage[ mainQUEUE_ITEM_SIZE * mainQUEUE_LENGTH ];

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

int main(void)
{
  uart_init();
  stdin = stdout = stderr = &uart_file;

  // Create queue.
  xReadQueue = xQueueCreateStatic(
    mainQUEUE_LENGTH,
    mainQUEUE_ITEM_SIZE,
    ucReadQueueStorage,
    &xStaticReadQueue
  );
  
  // Create tasks.
  xTaskHandle led_handle;
  led_handle = xTaskCreateStatic(
    vLedCode,
    "led",
    configMINIMAL_STACK_SIZE,
    NULL,
    mainLED_TASK_PRIORITY,
    xLedStack,
    &xLedBuffer
  );


  xTaskHandle read_handle;
  read_handle = xTaskCreateStatic(
    vReadCode,
    "read",
    configMINIMAL_STACK_SIZE,
    NULL,
    mainREAD_TASK_PRIORITY,
    xReadStack,
    &xReadBuffer
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

static void vReadCode(void* pvParameters) {
  uint16_t input;
  while(1) {
    printf("Enter number of ms: ");
    scanf("%"SCNu16, &input);
    xQueueSend(xReadQueue, &input, portMAX_DELAY);
  }
}

#define led_on()  mainLED_PORT |=  _BV(mainLED)
#define led_off() mainLED_PORT &= ~_BV(mainLED)

static void vLedCode(void* pvParameters) {
  uint16_t num_of_ms;
  mainLED_DDR |= _BV(mainLED);

  while (1) {
    xQueueReceive(xReadQueue, &num_of_ms, portMAX_DELAY);
    
    led_on();
    vTaskDelay( num_of_ms / portTICK_PERIOD_MS );
    led_off();
    vTaskDelay( 100 / portTICK_PERIOD_MS );
  }
}
/******************************************************************************
 * Header file inclusions.
 ******************************************************************************/

#include "FreeRTOS.h"
#include "task.h"

#include <avr/io.h>

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

#define mainKITT_TASK_PRIORITY   2

#define mainDELAY_TASK_PRIORITY  1

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vKittCode(void* pvParameters);
static void vDelayCode(void* pvParameters);

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

StaticTask_t xKittBuffer;
StackType_t xKittStack[ configMINIMAL_STACK_SIZE ];

StaticTask_t xDelayBuffer;
StackType_t xDelayStack[ configMINIMAL_STACK_SIZE ];

int main(void)
{
    // Create task.
    xTaskHandle kitt_handle;
    xTaskHandle delay_handle;

    kitt_handle = xTaskCreateStatic(
      vKittCode,
      "kitt",
      configMINIMAL_STACK_SIZE,
      NULL,
      mainKITT_TASK_PRIORITY,
      xKittStack,
      &xKittBuffer
    );

    delay_handle = xTaskCreateStatic(
      vDelayCode,
      "delay",
      configMINIMAL_STACK_SIZE,
      NULL,
      mainDELAY_TASK_PRIORITY,
      xDelayStack,
      &xDelayBuffer
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

#define mainBAR_SIZE         8
#define mainBAR_DDR          DDRD
#define mainBAR_DISPLAY_PORT PORTD

static void vKittCode(void* pvParameters) {
  // printf("here\r\n");
  uint8_t state = 1;
  UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
  mainBAR_DDR = 0xFF;
  mainBAR_DISPLAY_PORT = state;

  while(1) {
    for(int i=0; i < mainBAR_SIZE - 1; i++){
      vTaskDelay(80 / portTICK_PERIOD_MS);
      state <<= 1;
      mainBAR_DISPLAY_PORT = state;
    }
    for (int i = 0; i < mainBAR_SIZE - 1; i++) {
      vTaskDelay(80 / portTICK_PERIOD_MS);
      state >>= 1;
      mainBAR_DISPLAY_PORT = state;
    }
  }
}

#define mainLED      PC0
#define mainLED_DDR  DDRC
#define mainLED_PORT PORTC

#define mainBTN      PC2
#define mainBTN_PIN  PINC
#define mainBTN_PORT PORTC 

#define led_on()  mainLED_PORT |=  _BV(mainLED)
#define led_off() mainLED_PORT &= ~_BV(mainLED)

#define mainCYCLIC_BUFFER_SIZE 100

typedef int8_t cyclic_buffer[mainCYCLIC_BUFFER_SIZE];
static cyclic_buffer buffer = {0};
static uint8_t index = 0;

static void next() {
  index = (index == mainCYCLIC_BUFFER_SIZE - 1) ? 0 : (index + 1);
}

static void write(int8_t val) {
  buffer[index] = val;
}

static int8_t read() {
  return buffer[index];
}

static void vDelayCode(void* pvParameters) {

  mainBTN_PORT |= _BV(mainBTN);
  mainLED_DDR  |= _BV(mainLED);
  led_on();


  while(1) {
    if(read()) led_on();
    else       led_off();

    if(mainBTN_PIN & _BV(mainBTN)) write(0);
    else
    write(_BV(mainLED));

    vTaskDelay(10 / portTICK_PERIOD_MS);
    next();
  }
}
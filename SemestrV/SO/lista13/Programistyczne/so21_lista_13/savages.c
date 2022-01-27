#include "csapp.h"

static void outc(char c) {
  Write(STDOUT_FILENO, &c, 1);
}

#define N 100
#define M 100

static struct {
  /* TODO: Put semaphores and shared variables here. */
  /* będziemy przechowywać liczbę porcji w kotle */
  int pot_state;
  /* dostępu do licznika będzie chronił semafor binarny */
  sem_t mutex;
  /* będziemy używać semaforów binarnych do sygnalizowania,
   * że kocioł jest już pełny/pusty */
  sem_t pot_full;
  sem_t pot_empty;
} *shared = NULL;


static void savage(void) {
  for (;;) {
    /* TODO Take a meal or wait for it to be prepared. */

    /* Uzyskaj dostęp do shared */
    Sem_wait(&shared->mutex);

    if(shared->pot_state == 0){
      /* Kocioł pusty, budzimy kucharza */
      Sem_post(&shared->pot_empty);
      /* Czekamy, aż się napełni */
      Sem_wait(&shared->pot_full);
    }
    /* Zjadamy porcję gulaszu */
    outc('s');
    shared->pot_state--;

    /* Zwolnij blokadę shared */
    Sem_post(&shared->mutex);

    /* Sleep and digest. */
    usleep(rand() % 1000 + 1000);
  }

  exit(EXIT_SUCCESS);
}

static void cook(void) {
  for (;;) {
    /* TODO Cook is asleep as long as there are meals.
     * If woken up they cook exactly M meals. */

    /* Czekaj, aż ktoś zasygnalizuje, że kocioł jest pusty */
    Sem_wait(&shared->pot_empty);
    outc('c');
    /* Napełnij kocioł, alternatywnie możemy to zrobić w savage, żeby
     * była jasność, że jest to robione pod mutexem, inaczej trzeba
     * trochę pomachać */
    shared->pot_state = M;
    /* Zasygnalizuj, że kocioł jest pełny */
    Sem_post(&shared->pot_full);
  }
}

/* Do not bother cleaning up after this process. Let's assume that controlling
 * terminal sends SIGINT to the process group on CTRL+C. */
int main(void) {
  shared = Mmap(NULL, getpagesize(), PROT_READ|PROT_WRITE, MAP_ANON|MAP_SHARED,
                -1, 0);

  /* TODO: Initialize semaphores and other shared state. */
  shared->pot_state = 0;
  Sem_init(&shared->mutex, 1, 1);
  Sem_init(&shared->pot_full, 1, 0);
  Sem_init(&shared->pot_empty, 1, 0);

  for (int i = 0; i < N; i++)
    if (Fork() == 0)
      savage();

  cook();

  return EXIT_SUCCESS;
}

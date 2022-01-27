#include "csapp.h"

static __unused void outc(char c) {
  Write(STDOUT_FILENO, &c, 1);
}

static void randsleep(void) {
  usleep(rand() % 5000 + 5000);
}

#define N 3 

static pthread_t td[N];
static sem_t forks[N];
/* TODO: If you need extra shared state, define it here. */
static sem_t seats;

void *philosopher(void *id) {
  int right = (intptr_t)id;
  int left = right == 0 ? N - 1 : right - 1;

  for (;;) {
    /* Think */
    printf("Philosopher %d thinking\r\n", right);
    randsleep();

    /* TODO: Take forks (without deadlock & starvation) */
    printf("Philosopher %d tries to take a seat\r\n", right);
    Sem_wait(&seats);

    printf("Philosopher %d acquiring right fork\r\n", right);
    Sem_wait(&forks[right]);

    randsleep();

    printf("Philosopher %d acquiring left fork\r\n", right);
    Sem_wait(&forks[left]);

    printf("Philosopher %d eating\r\n", right);
    /* Eat */
    randsleep();

    /* TODO: Put forks (without deadlock & starvation) */
    Sem_post(&forks[left]);
    Sem_post(&forks[right]);
    Sem_post(&seats);
  }

  return NULL;
}

int main(void) {
  /* TODO: If you need extra shared state, initialize it here. */
  Sem_init(&seats, 0, N - 1);

  for (int i = 0; i < N; i++)
    Sem_init(&forks[i], 0, 1);

  for (int i = 0; i < N; i++)
    Pthread_create(&td[i], NULL, philosopher, (void *)(intptr_t)i);

  for (int i = 0; i < N; i++)
    Pthread_join(td[i], NULL);
  
  return EXIT_SUCCESS;
}

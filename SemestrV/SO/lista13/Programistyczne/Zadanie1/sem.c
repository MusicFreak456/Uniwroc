#include<pthread.h>

typedef struct Sem {
  pthread_mutex_t mutex;
  pthread_cond_t waiters;
  int value;
} Sem_t;

void init(Sem_t * semaphore, int value) {
  pthread_mutex_init(&semaphore->mutex, NULL);
  pthread_cond_init(&semaphore->waiters, NULL);
  semaphore->value = value;
}

void wait(Sem_t * semaphore) {
  pthread_mutex_lock(&semaphore->mutex);
  while(semaphore->value == 0) {
    pthread(&semaphore->waiters, &semaphore->mutex);
  }
  semaphore->value--;
  pthread_mutex_unlock(&semaphore->mutex);
}

void post(Sem_t * semaphore) {
  pthread_mutex_lock(&semaphore->mutex);
  semaphore->value++;
  pthread_cond_signal(&semaphore->waiters);
  pthread_mutex_unlock(&semaphore->mutex);
}
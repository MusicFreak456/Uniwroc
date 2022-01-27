#include "csapp.h"

static __unused void outc(char c) {
  Write(STDOUT_FILENO, &c, 1);
}

typedef struct {
  /* TODO: Use this structure to store barrier internal state. */

  /* Liczba wątków wpuszczanych na raz */
  int n;
  /* Liczba wątków pomiędzy blokadami */
  int slots_taken;
  /* Semafor synchronizujący dostęp do liczników */
  sem_t mutex;
  /* Pierwsza i druga blokada */
  sem_t first_blockade;
  sem_t second_blockade;

} barrier_t;

static barrier_t *barrier_init(int n) {
  if (n < 1)
    app_error("barrier_init requires n > 0");

  barrier_t *b = Mmap(NULL, sizeof(barrier_t), PROT_READ|PROT_WRITE,
                      MAP_ANON|MAP_SHARED, -1, 0);

  /* TODO: Initialize barrier internal state. */
  Sem_init(&b->first_blockade, 1, n);
  Sem_init(&b->second_blockade, 1, 0);
  Sem_init(&b->mutex, 1, 1);
  b->n = n;
  b->slots_taken = 0;

  return b;
}

static void barrier_wait(barrier_t *b) {
  /* TODO: Provide wait procedure implementation here. */

  /* Pierwsza blokada wpuści co najwyżej N koni */
  Sem_wait(&b->first_blockade);

  Sem_wait(&b->mutex);
  b->slots_taken++;
  if(b->slots_taken == b->n) {
    /* Przestrzeń między blokadami się zapełniła */
    /* Można otwierać drugą blokadę */
    for(int i = 0; i < b->n; i++) {
      Sem_post(&b->second_blockade);
    }
  }
  Sem_post(&b->mutex);


  /* Blokujemy się na drugiej blokadzie */
  Sem_wait(&b->second_blockade);
  /* Jesteśmy wypuszczeni z bariery */

  Sem_wait(&b->mutex);
  b->slots_taken--;
  if(b->slots_taken == 0) {
    /* Przestrzeń między barierami jest pusta, druga blokada jest zamknięta,
     * można bezpiecznie zacząć przepuszczać konie przez pierwszą blokadę.*/
    for (int i = 0; i < b->n; i++) {
      Sem_post(&b->first_blockade);
    }
    
  }
  Sem_post(&b->mutex);
}

static void barrier_destroy(barrier_t *b) {
  /* TODO: Provide destroy procedure implementation here. */
  Sem_destroy(&b->first_blockade);
  Sem_destroy(&b->second_blockade);
  Sem_destroy(&b->mutex);

  Munmap(b, sizeof(barrier_t));
}

#define K 100
#define N 50
#define P 100

static noreturn void horse(barrier_t *b) {
  int n = rand() % K + K;

  outc('+');

  for (int i = 0; i < n; i++) {
    barrier_wait(b);
    usleep(rand() % 2000 + 1000);
  }

  outc('-');

  exit(EXIT_SUCCESS);
}

/* Do not bother cleaning up after this process. Let's assume that controlling
 * terminal sends SIGINT to the process group on CTRL+C. */
int main(void) {
  barrier_t *b = barrier_init(N);

  int horses = 0;

  for (;;) {
    do {
      if (Fork() == 0) {
        srand(getpid());
        horse(b);
      }
      horses++;
    } while (horses < P);

    Wait(NULL);
    horses--;
  }

  barrier_destroy(b);

  return EXIT_SUCCESS;
}

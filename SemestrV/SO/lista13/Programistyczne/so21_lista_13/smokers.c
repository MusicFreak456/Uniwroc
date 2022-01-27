#include "csapp.h"

static __unused void outc(char c) {
  Write(STDOUT_FILENO, &c, 1);
}

static __thread unsigned seed;

static sem_t tobacco;
static sem_t matches;
static sem_t paper;
static sem_t doneSmoking;

/* TODO: If you need any extra global variables, then define them here. */
/* semafor binarny chroniacy odczytywanie globalnego stanu */
static sem_t observe_resource_lock;
/* semafory, którymi wybudzać będziemy palaczy */
static sem_t tobacco_smoker;
static sem_t matches_smoker;
static sem_t paper_smoker;

typedef enum {
  TOBACCO = 0,
  MATCHES = 1,
  PAPER = 2
} resource_t;
/* stan dostępności zasobów, 1 - dostępny, 0 - niedostępny */
static bool resources[3];

static void *agent(void *arg) {
  seed = pthread_self();

  while (true) {
    Sem_wait(&doneSmoking);

    int choice = rand_r(&seed) % 3;
    if (choice == 0) {
      Sem_post(&tobacco);
      Sem_post(&paper);
    } else if (choice == 1) {
      Sem_post(&tobacco);
      Sem_post(&matches);
    } else {
      Sem_post(&paper);
      Sem_post(&matches);
    }
  }

  return NULL;
}

/* TODO: If you need extra threads, then define their main procedures here. */

static void arbiter(void) {

  if( resources[TOBACCO] & resources[MATCHES] ) {
    /* tytoń i zapałki dostępne, budzimy palacza z bibułkami */
    sem_post(&paper_smoker);
  }

  if( resources[TOBACCO] & resources[PAPER] ) {
    /* tytoń i bibułki dostępne, budzimy palacza z zapałkami */
    sem_post(&matches_smoker);
  }

  if( resources[MATCHES] & resources[PAPER] ) {
    /* Zapałki i papier dostępne, budzimy palacza z tytoniem */
    sem_post(&tobacco_smoker);
  }

  if(resources[TOBACCO] + resources[PAPER] + resources[MATCHES] >= 2) {
    /* Obudzimy jakiegoś palacza, więc oznaczamy zasoby jako niedostępne */
    resources[TOBACCO] = resources[PAPER] = resources[MATCHES] = 0;
  }
}

static noreturn void *resource_observer(void *argp) {
  resource_t observed_resource = (resource_t)argp;

  while (true) {
    /* Czekamy na pojawienie się konkretnego zasobu */
    switch (observed_resource) {
    case TOBACCO:
      sem_wait(&tobacco);
      break;
    case PAPER:
      sem_wait(&paper);
      break;
    case MATCHES:
      sem_wait(&matches);
      break;
    }
    sem_wait(&observe_resource_lock);

    /* Oznaczamy go jako dostępny */
    resources[observed_resource] = 1;
    /* Sprawdzamy czy należy obudzić palacza */
    arbiter();

    sem_post(&observe_resource_lock);
  }
}

static void randsleep(void) {
  usleep(rand_r(&seed) % 1000 + 1000);
}

static void make_and_smoke(char smoker) {
  randsleep();
  Sem_post(&doneSmoking);
  outc(smoker);
  randsleep();
}

static void *smokerWithMatches(void *arg) {
  seed = pthread_self();

  while (true) {
    /* TODO: wait for paper and tobacco */
    sem_wait(&matches_smoker);
    make_and_smoke('M');
  }

  return NULL;
}

static void *smokerWithTobacco(void *arg) {
  seed = pthread_self();

  while (true) {
    /* TODO: wait for paper and matches */
    sem_wait(&tobacco_smoker);
    make_and_smoke('T');
  }

  return NULL;
}

static void *smokerWithPaper(void *arg) {
  seed = pthread_self();
 
  while (true) {
    /* TODO: wait for tobacco and matches */
    sem_wait(&paper_smoker);
    make_and_smoke('P');
  }

  return NULL;
}

int main(void) {
  Sem_init(&tobacco, 0, 0);
  Sem_init(&matches, 0, 0);
  Sem_init(&paper, 0, 0);
  Sem_init(&doneSmoking, 0, 1);

  /* TODO: Initialize your global variables here. */
  Sem_init(&observe_resource_lock, 0, 1);
  Sem_init(&tobacco_smoker, 0, 0);
  Sem_init(&matches_smoker, 0, 0);
  Sem_init(&paper_smoker, 0, 0);

  pthread_t tobacco_observer, paper_observer, matches_observer;
  Pthread_create(&tobacco_observer, NULL, resource_observer, (void*)TOBACCO);
  Pthread_create(&paper_observer, NULL, resource_observer, (void*)PAPER);
  Pthread_create(&matches_observer, NULL, resource_observer, (void*)MATCHES);

  pthread_t agentThread;
  Pthread_create(&agentThread, NULL, agent, NULL);

  pthread_t smokerPaperThread, smokerMatchesThread, smokerTobaccoThread;
  Pthread_create(&smokerPaperThread, NULL, smokerWithPaper, NULL);
  Pthread_create(&smokerMatchesThread, NULL, smokerWithMatches, NULL);
  Pthread_create(&smokerTobaccoThread, NULL, smokerWithTobacco, NULL);

  Pthread_join(agentThread, NULL);
  Pthread_join(smokerPaperThread, NULL);
  Pthread_join(smokerMatchesThread, NULL);
  Pthread_join(smokerTobaccoThread, NULL);

  return 0;
}

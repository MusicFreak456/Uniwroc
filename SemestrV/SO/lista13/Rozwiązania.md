# Systemy Operacyjne -- Lista 13
###### tags: `SO`

## Zadanie 1
![](https://i.imgur.com/JuqTHue.png)

```c=
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
  // while jest potrzebny na wypadek gdyby ktoś zajął
  // zasób zanim wątek uruchomi się po wyjściu ze snu
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
```

## Zadanie 2
![](https://i.imgur.com/DRy2HZB.png)

### Opis mechanizmu

Mechanizm *futex* (skrót od  *fast user space mutex*) to najbardziej podstawowy mechanizm synchronizacji w systemie linux, za pomocą którego implementuje się inne. Główną zaletą tego mechanizmu jest unikanie używania wywołań systemowych, które są zbyt kosztowne w warunkach niskiego współzawodnictwa.
Składa się z dwóch części:

* usługa jądra systemu, która udostępnia wywołanie systemowe `futex(2)`, za pomocą którego możemy przenosić wątki w stan uśpienia w oczekiwaniu na zdarzenie.
```c=
int futex(int *uaddr, int futex_op, int val, 
          const struct timespec *timeout, /* or: uint32_t val2 */
          int *uaddr2, int val3);
```
* biblioteka po stronie użytkownika, w której synchronizacja polega na przypisaniu 32-bitowej zmiennej (nazywanej *futex word*) znaczenia i operowaniu na niej operacjami atomowymi w celu ustalenia czy blokada jest wolna, czy może trzeba wątek uśpić (czego będziemy się starać unikać).

Argumenty przyjmowane przez wywołanie systemowe to:

* `int *uaddr` -- wskaźnik na *futex word*.
* `int futex_op` -- operacja jaka ma być wykonana na futeksie.
* `int val` -- wartość, której znaczenie zależy od operacji.
* `timeout/val2, uaddr2, val3` -- argumenty opcjonalne, wymagane przez niektóre operacje.

#### Operacje FUTEX_WAIT i FUTEX_WAKE

Są to dwie podstawowe operacje na futexach.

* `FUTEX_WAIT` atomowo sprawdza czy wartość futexa pod wskaźnikiem `uaddr` jest taka sama jak `val` i jeśli tak, to usypia wątek na futeksie, aż do otrzymania na nim `FUTEX_WAKE`. Jeśli wartość jest różna, to zwraca od razu z błędem `EAGAIN`. Cel porównania jest taki sam jak z `setpark` na poprzedniej liście -- chcemy unkinąć lost-wakeups. Opcjonalnie można ustalić `timeout` na długość czekania.
* `FUTEX_WAKE` budzi co najmniej `val` wątków oczekujących na futeksie wskazanym przez `uaddr`. Nie można nic powiedzieć o kolejności w jakiej budzonę są wątki.

### Czym się wyróżniają blokady adaptacyjne?

W przeciwieństwie do blokad usypiających nie posiadają jednej strategii oczekiwania na zwolnienie zasobu i mogą wybierać metody inne niż blokowanie (np. spin) jeśli niewiele wątków walczy o ten zasób.

### Przykładowa implementacja blokady

#### Znaczenie wartości futeksa

```c= 
/* This lock primitive combines a flag (in the sign bit) and a
 * congestion count (= threads inside the critical section, CS) in a
 * single int that is accessed through atomic operations. The states
 * of the int for value x are:
 *
 * x == 0: unlocked and no thread inside the critical section
 *
 * x < 0: locked with a congestion of x-INT_MIN, including the thread
 * that holds the lock
 *
 * x > 0: unlocked with a congestion of x
 *
 * or in an equivalent formulation x is the congestion count or'ed
 * with INT_MIN as a lock flag.
 */
```

Tak więc wartość najmniej znaczących 31 bitów futeksa to liczba wątków na nim uśpionych, a na 32 bicie znajduje się flaga mówiąca, czy blokada jest zajęta (wartość `0`) czy wolna (wartość `2`).

#### Lock

```c=
void __lock(volatile int *l)
{
  /* Jeśli blokada była wolna i nikt na nią nie czekał, 
   * to oznaczamy ją jako zajętą i ustawiamy licznik 
   * zainteresowania na 1 */
  int current = a_cas(l, 0, INT_MIN + 1);
  if (!current) return;

  /* Próbujemy spin lockować parę razy, żeby uniknąć 
   * usypiania wątku wywołaniem systemowym */
  for (unsigned i = 0; i < 10; ++i) {
    /* Jeśli blokada jest zajęta i się zwolni
     * to będzie o 1 mniej wątków czekających 
     * i najstarszy bit będzie zerem */
    if (current < 0) current -= INT_MIN + 1; 
    /* od tego miejsca: current >= 0
    /* Jeśli się zwolniła, lub nie była zajęta
     * to zwiększamy licznik zainteresowania o 1 i 
     * oznaczamy blokadę jako zajętą */
    int val = a_cas(l, current, INT_MIN + (current + 1));
    /* Jeśli się udało to zwracamy od razu */
    if (val == current) return;
    /* Jeśli nie, to kręcimy się dalej */
    current = val;
  }
  
  /* Jeśli nie udało nam się pozyskać blokady spin lockiem
   * to dodajemy się do liczby wątków oczekujących na blokadę */
  current = a_fetch_add(l, 1) + 1;
  /* Skoro spinowanie nie dało efektów, to uzasdnionym jest
   * użycie wywołania systemowego do uśpienia wątku */
  for (;;) {
    if (current < 0) {
      /* Blokadę dalej ktoś przetrzymuje, idziemy spać */
      __futexwait(l, current, 1);
      /* Jeśli blokada się zwolni to będzie o 1 mniej 
       * wątków czekających i najstarszy bit będzie zerem */
      current -= INT_MIN + 1;
    }
    /* Od tego miejsca: current > 0, 
     * jesteśmy już wliczeni w licznik zainteresowania */
    int val = a_cas(l, current, INT_MIN + current);
    /* Jeśli się udało pozyskać blokadę, to zwracamy */
    if (val == current) return;
    /* Jeśli nie, to powtarzamy pętlę, więc ponownie zaśniemy */
    current = val;
  }
}
```

#### Unlock

```c=
void __unlock(volatile int *l)
{
  /* Check l[0] to see if we are multi-threaded. */
  if (l[0] < 0) {
    if (a_fetch_add(l, -(INT_MIN + 1)) != (INT_MIN + 1)) {
      /* Jeśli nie byliśmy jedynym czekającym wątkiem, to budzimy inny */
      __wake(l, 1, 1);
    }
  }
}
```

#### Futexwait i wake

```c=
static inline void __wake(volatile void *addr, int cnt, int priv)
{
	if (priv) priv = FUTEX_PRIVATE;
	if (cnt<0) cnt = INT_MAX;
	__syscall(SYS_futex, addr, FUTEX_WAKE|priv, cnt) != -ENOSYS ||
	__syscall(SYS_futex, addr, FUTEX_WAKE, cnt);
}
static inline void __futexwait(volatile void *addr, int val, int priv)
{
	if (priv) priv = FUTEX_PRIVATE;
	__syscall(SYS_futex, addr, FUTEX_WAIT|priv, val, 0) != -ENOSYS ||
	__syscall(SYS_futex, addr, FUTEX_WAIT, val, 0);
}
```

### Jak blokada zachowuje się w warunkach wysokiego współzawodnictwa?

Zaczyna częściej usypiać wątki, gdyż spin lockowanie będzie miało tam mniejszą szansę powodzenia. 

### W jakich warunkach budzimy i usypiamy wątki

* Wątki są usypiane tylko, jeśli blokada jest zajęta, ktoś na nią czeka i nie udało jej się pozyskać przez spin lock'a, ma to na celu używanie wywołań systemowych tylko w ostateczności, w uzasadnionym przypadku.
* Co najmniej jeden wątek jest wybudzany, jeśli niższe 31 bitów futexa, było różne od 1, czyli na blokadę czekały jakieś wątki (wpp. ta jedynka to wątek, który tą blokadę teraz zwalnia).

## Zadanie 3

![](https://i.imgur.com/LEHBbWD.png)

### Znaczenie zmiennych

```python=
mutex = semaphore(1) # implementuje sekcję krytyczną
block = semaphore(0) # oczekiwanie na opuszczenie zasobu
active = 0 # liczba użytkowników zasobu
waiting = 0 # liczba użytkowników oczekujących na zasób
must_wait = False # czy kolejni użytkownicy muszą czekać?
```

### Kod rozwiązania

```python=
def acquire():
  mutex.wait()
  if must_wait: # czy while coś zmieni?
    waiting += 1
    mutex.post()
    block.wait()
    mutex.wait()
    waiting -= 1
  active += 1
  must_wait = (active == 3)
  mutex.post()
  
def release():
  mutex.wait()
  active -= 1
  if active == 0:
    n = min(waiting, 3);
    while n > 0:
      block.post()
      n -= 1
    must_wait = False
  mutex.post()
```

### Kontrprzykład 1

#### Przeplot

* Wątki $P1$, $P2$, $P3$ wykonują w całości `acquire`.
* Wątek $P4$ też chce zrobić `acquire()` i usypia na `block.wait()`.
* Wątki $P1$, $P2$, $P3$ robią `release`, wybudzając wątek $P4$.
* Wątki $P5$, $P6$, $P7$ wykonują w całości `acquire`.
* Wątrk $P4$ wznawia dziaąnie i wchodzi do sekcji krytycznej.

#### Problemy

* W sekcji krytycznej znalazły się 4 wątki na raz.
* Wątki $P5$, $P6$, $P7$, uzyskały dostęp zanim dostęp dostał $P4$ (i while tego nie zmieni).

### Kontrprzykład 2

* Wątki $P1$, $P2$, $P3$ wykonują w całości `acquire`.
* Wątek $P4$ też chce zrobić `acquire()` i usypia na `block.wait()`.
* Wątki $P1$, $P2$, $P3$ robią `release`, wybudzając wątek $P4$.
* Wątki $P5$, $P6$, $P7$ wykonują w całości `acquire`.
* Wątki $P5$, $P6$, $P7$ wykonują w całości `release`, wartość semafora `block` to teraz jeden, bo znowu został zwiększony, gdyż $P4$ wciąż nie zmniejszył `waiting`.
* $P4$ wchodzi do sekcji krytycznej
* Wątki $P8$, $P9$ wykonują w całości `acquire`.
* Wątek $P10$ wykonuje `acquire`, wchodzi do `if'a`, ale nie blokuje się na `block`, gdyż jego wartość jest wciąż zwiększona.

#### Problemy

Te same co w poprzednim przypadku.

## Zadanie 4

![](https://i.imgur.com/kGUCtiF.png)


### Idea rozwiązania

Zauważmy, że jeśli jest więcej miejsc i pałeczek, niż filozofów (jest 1 puste miejsce przy stole), to nawet jeśli każdy filozof weźmie po jednej pałeczce, to wciąż zostaje jedna pałeczka, której nikt nie wziął i która sąsiaduje z jakimś obecnym filozofem i ten filozof będzie mógł rozpocząć jedzenie.

Fakt ten da sie wykorzystać ograniczając liczbę filozofów, którzy konkurują o pałeczki semaforem. Filozof konkurujący o możliwość uczestniczenia w bitwie o zasoby kiedyś dostanie tą możliwość, gdyż zakładamy że implementacja semafora jest sprawiedliwa, podobnie z konkurowaniem o pałeczki.

### Kod rozwiązania

```c=
static pthread_t td[N];
static sem_t forks[N];
/* TODO: If you need extra shared state, define it here. */
/* semafor seats będzie pilnował żeby o pałeczki konkurowało
 * max N - 1 filozofów */
static sem_t seats;

void *philosopher(void *id) {
  int right = (intptr_t)id;
  int left = right == 0 ? N - 1 : right - 1;

  for (;;) {
    /* Think */
    randsleep();

    /* TODO: Take forks (without deadlock & starvation) */
    /* spróbuj zająć miejsce przy stole */
    Sem_wait(&seats);
    /* spróbuj wziąć pałeczki */
    Sem_wait(&forks[right]);
    Sem_wait(&forks[left]);

    /* Eat */
    randsleep();

    /* TODO: Put forks (without deadlock & starvation) */
    /* odłóż pałeczki */
    Sem_post(&forks[left]);
    Sem_post(&forks[right]);
    /* zwolnij miejsce */
    Sem_post(&seats);
  }

  return NULL;
}

int main(void) {
  /* TODO: If you need extra shared state, initialize it here. */
  /* Dopuszczamy, żeby przy stole zasiadło max N - 1 filozofów */
  Sem_init(&seats, 0, N - 1);

  for (int i = 0; i < N; i++)
    Sem_init(&forks[i], 0, 1);

  for (int i = 0; i < N; i++)
    Pthread_create(&td[i], NULL, philosopher, (void *)(intptr_t)i);

  for (int i = 0; i < N; i++)
    Pthread_join(td[i], NULL);
  
  return EXIT_SUCCESS;
}
```

## Zadanie 5
![](https://i.imgur.com/iOGfvdC.png)

### Kod rozwiązania

```c=
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
```

## Zadanie 6

![](https://i.imgur.com/n0IsvW5.png)

### Idea rozwiązania

Idea będzie bazowana na konstrukcji śluzy wodnej. Obecne są dwie blokady:

* blokada pierwsza wpuszcza dokładnie $N$ pierwszych wątków do sekcji między blokadami i zamyka się. Zostanie ponownie otwarta kiedy ostatni wątek opuści sekcję między blokadami.
* blokada druga zatrzymuje wątki wchodzące do sekcji między blokadami aż znajdzie się w niej dokładnie $N$ wątków, wtedy zostaje otwarta i wpuszcza wątki do sekcji krytycznej. 

### Stan bariery i semafory

```c=
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
```

#### Inicjalizacja

```c=
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
```

### Implementacja bariery

```c= 
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
```

## Zadanie 7

![](https://i.imgur.com/V6PrNaf.png)

### Idea rozwiązania

Będą uruchomione dodatkowe wątki-nasłuchiwacze, które będą oczekiwać na pojawienie się konkretnego zasobu, kiedy się pojawi, to zaznaczą jego obecność w globalnej tablicy i sprawdzą czy są już obecne dwa zasoby. Jeśli tak to obudzą odpowiedni wątek, który je wykorzysta. Jeśli nie to zaczną oczekiwać dalej.


### Globalny stan i semafory

```c=
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
```

#### Inicjalizacja

```c=
Sem_init(&observe_resource_lock, 0, 1);
Sem_init(&tobacco_smoker, 0, 0);
Sem_init(&matches_smoker, 0, 0);
Sem_init(&paper_smoker, 0, 0);
```

### Kod wykonywany przez nasłuchiwaczy

```c=
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
```

#### Inicjalizacja

```c=
pthread_t tobacco_observer, paper_observer, matches_observer;
Pthread_create(&tobacco_observer, NULL, resource_observer, (void*)TOBACCO);
Pthread_create(&paper_observer, NULL, resource_observer, (void*)PAPER);
Pthread_create(&matches_observer, NULL, resource_observer, (void*)MATCHES);
```

### Kod wykonywany przez palaczy

```c=
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
```
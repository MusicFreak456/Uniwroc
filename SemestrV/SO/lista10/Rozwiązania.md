# Systemy operacyjne -- Lista 10
###### tags: `SO`

## Zadanie 1

### Przetwarzanie równoległe

Sposób przetwarzania instrukcji wielu wątków/procesów, w którym ich wykonanie zachodzi na prawdę jednocześnie, np. na kilku procesorach.

![](https://i.imgur.com/dlNyrvm.png)

### Przetwarzanie współbieżne

Inny sposób przetwarzania instrukcji wielu wątków, w którym okresy czasu ich wykonania nie są rozłączne, ale w żadnym momencie nie wykonywane są na raz instrukcje dwóch wątków.

![](https://i.imgur.com/uWxGVCy.png)

Jeśli czas wykonania instrukcji dwóch wątków jest rozłączny (np. wątek B i C na obrazku) to jest to przetwarzanie sekwencyjne.

### Czym charakteruzyją się wielobieżne procedury?

Definicji jest wiele.

W tym rozwiązaniu zdecydowałem się przyjąć tą zamieszczoną w podręczniku *Computer Systems: A Programmer's Perspective*, który definiuje [12.7.1]:

* bezpieczeństwo wątkowe (thread safety), czyli własność funkcji mówiąca, że może być ona wołana jednocześnie i wielokrotnie z wielu wątków, i zawsze poprawnie i w oczekiwany sposób wykona swoje działanie. Jeśli funkcja ma tą własność mówimy że jest `thread-safe` (atrybut `MT-Safe`), jeśli nie ma to mówimy że jest `thread-unsafe` (atrybut `MT-Unsafe`).
* procedury wielobieżne -- podzbiór procedur pochodzcych ze zbioru procedur bezpiecznych wątkowo, które nie odwołują się nigdy do współdzielonego stanu, a co za tym idzie nie muszą być synchronizowane (np. blokadami).

Interesującym nas pojęciem mogą być też procedury `async-signal-safe` (atrybut AS-Safe), czyli takie które można bezpiecznie wołać z procedur obsługi sygnału. Ta własność może być osiągana zazwyczaj na dwa sposoby: albo procedura jest wielobieżna, albo nie może być przerwana przez sygnał.

### Procedura wielobieżna, ale nie wątkowo-bezpieczna

Nie jest to możliwe, zgodnie z definicją, procedury wielobieżne są podzbiorami procedur wątkowo bezpiecznych:

![](https://i.imgur.com/R5uHVPU.png)

Istnieją za to procedury `async-signal-safe`, które nie są bezpieczne wątkowo, jest to np. `signal`, którego zachowanie jest niezdefiniowane w środowisku, w którym działa wiele wątków.

### Procedura wątkowo-bezpieczna, ale nie wielobieżna

Przykładem z języka C może być procedura `printf`, odwołuje ona się do globalnych zasobów w postaci bufora wyjścia.

### Kiedy w jednowątkowym procesie może wystąpić współbieżność?

Np. kiedy proces w trakcie działania otrzymuje sygnał i ma zarejestrowaną procedurę obsługi tego sygnału, to wtedy wykonana jest ona współbieżnie w stosunku do reszty programu.

Innym przykładem może być sytuacja w której sami sobie strugamy tą współbieżność np. używając współprogramów (coroutines) tak jak w zadaniu z którejś z poprzednich list, albo pisząc serwer oparty o zdarzenia.

## Zadanie 2

(Na podstawie *Modern Operating Systems* Andrew S. Tanenbaum, Herbert Bos)

### Zakleszczenie -- deadlock

Występuje w momencie kiedy wątek ma wyłączny dostęp do zasobu `x` oraz czeka na zwolnienie innego zasobu `y`, do którego wyłączny dostęp ma inny wątek czekający na zwolnienie zasobu `x` (oczywiście to minimalny przypadek, taka cykliczna zależność może tworzyć się pomiędzy dowolną liczbą wątków). Wszystkie wątki są w stanie zablokowanym.

#### Analogia w ruchu ulicznym

Na skrzyżowaniu równorzędnym 4 auta podjeżdżają z każdego kierunku jednocześnie i chcą kontynuować jazdę prosto. W cyklicznej zależności każdy czeka na ruch auta po prawej, co nigdy się nie wydarzy.

![](https://i.imgur.com/9Y9GVGH.png)

### Uwięzienie -- livelock

Sytuacja podobna do zakleszczenia, różnica polega na tym, że wątki nie blokują się, tylko np. ciągle przekazują sobie nawzajem zasób w reakcji na to, że drugi wątek go potrzebuje.

Czyli np. wątek `A` posiada zasób `x` na wyłączność i potrzebuje zasobu `y`, ale jeśli zauważy że inny wątek `B` posiada zasób `y`, to odpuszcza wszystkie zasoby jakie posiada i próbuje je wszystkie jeszcze raz pozyskać po chwili. Wydaje się, że mogłoby to zapobiegać zakleszczeniom, ale powoduje problem uwiezięnia, jeśli wątek `B` będzie wykazywał takie same zachowanie w tym samym czasie (odda zasób `y` po tym jak `A` się poddał i pobierze go zanim `A` spróbuje jeszcze raz).

W tej sytuacji żaden wątek się nie blokuje, ale też żaden nie wykonuje postępu i tylko marnuje czas procesora na sprawdzanie.

#### Analogia w ruchu ulicznym

Wyobraźmy sobie zwężenie w drodze przez które może przejechać tylko jedno auto na raz. Z dwóch stron jednocześnie nadjeżdżają samochody `A` i `B`. Oba wykonują ruch w stronę zwężenia w tym samym czasie i oba ustępują widząc, że drugie auto zaczyna jechać.

![](https://i.imgur.com/xwHZg2a.png)

### Zagłodzenie -- starvation

Występuje kiedy w systamie mamy uprzywilejowane wątki, które nigdy nie pozwalają wykonywać się wątkom mniej uprzywilejowanym i w ten sposób zostają one zablokowane i nie mogą wykonywać żadnego postepu.

#### Analogia w ruchu ulicznym

Możemy wyobrazić sobie sytuację kiedy samochodą chcą wjechać z drogi podrzędnej, ale na drodze z pierwszeństwem samochody ciągle nadjeżdżają.

![](https://i.imgur.com/joeLrGW.png)

### Sposoby wykrywania 

Jeśli założymy dla uproszczenia, że mamy po jednym zasobie danego typu. Możemy zbudować graf zależności, w którym wierzchołkami będą zasoby oraz wątki, krawędź z zasobu `X` do wątku oznacza `A`, że `A` jest w posiadaniu zasobu `X`, a strzałka z wątku `A` do zasobu `X` oznacza, że `A` potrzebuje dostępu do zasobu `X`.

Przykładowy graf:

![](https://i.imgur.com/OE4LAlL.png)

Teraz sytuacja zakleszczenia objawi się obecnością cyklu w takim grafie, np w powyższym grafie zakleszczone są wątki D,E,G.

![](https://i.imgur.com/LSP8GHP.png)

Zatem problem wykrycia deadlocka, sprowadza się do utrzymywania takiego grafu zależności i wykrywania w nim cykli. Algorytm zaproponowany w podręczniku *Modern Operating Systems* polega na przeszukiwaniu wgłąb z każdego wierzchołka wraz z oznaczaniem odwiedzonych wierzchołków, bo powiedzieć że cykl istnieje jeśli jakiś zostanie odwiedzony dwa razy.

![](https://i.imgur.com/YqXwacd.png)

W bardziej złożonych przypadkach, algorytmy (bez zaskocznenia) są znacznie bardziej złożone. Przykład dla wielu instancji zasobów tego samego typu jest opisany w podręczniku *Modern Operating Systems*, ale pozwoliłem sobie go pominąć.

### Sposoby usuwania zakleszczeń

* Usunięcie przez wywłaszczenie -- Czasami, gdy wątek `A` prosi o zasób, który jest obecnie w posiadaniu innego wątku `B`, to można mu go siłą odebrać, np. jeśli on sam też jest w stanie oczekiwania na inny zasób.
* Usunięcie poprzez rollback (przewinięcie?) -- W systemach, w których wiadomo, że zakleszczenia mogą wystąpić z dużym prawdopodobieństwem, może opłacać się wykonywać migawkę stanu wątku przed zdobyciem zasobu, by po wykryciu zakleszczenia, móc go cofnąć do chwili zanim poprosił o zasób, a sam zasób zwrócić innym wątkom.
* Usunięcie poprzez zabicie wątku -- Prostym, dosyć skrajnym, ale skutecznym sposobem jest zabicie wątku który znalazł się z innymi w deadlocku. Jednak możliwe, że będzie trzeba zabić kilka wątków żeby odblokować pozostałe, a także nie zawsze wiadomo czy w cyklu znajdą się wątki, które faktycznie można bezpiecznie zabić i zrestartować bez niepożądanych skutków ubocznych.

### Sposoby zapobiegania zakleszczeniom

Można odwołać się do zaproponowanych przez Coffmana warunków, które muszą wystąpić w środowisku wielowątkowym, żeby zakleszczenie było możliwe:

* *Mutual-Exlusion* -- Muszą istnieć zasoby, które nie mogą być współdzielone przez wątki i tylko jeden wątek na raz może z nich korzystać. 
* *Hold and wait* -- Muszą istnieć procesy, które jednocześnie przetrzymują zasób konieczny do swojego wykonania oraz proszą o dostęp do kolejnych zasobów.
* *Non preemption* -- Zasób może zostać zwolniony tylko dobrowolnie przez wątek.
* *Circular wait* -- Wątki czekają w cyklicznej zależności na zasoby posiadane przez inne wątki.

Nie dopuszczając do wystąpienia chociaż jednego z powyższych warunków, możemy nie dopuścić do wystąpienia zakleszczenia, w praktyce jest to jednak trudne.


## Zadanie 3

Dany jest kod, w którym uruchamiamy wątki wykonujące kod procedury, wewnątrz której edytujemy wartość współdzielonej zmiennej `tally`.

```c=
const int n = 50;
shared int tally = 0;

void total() {
for (int count = 1; count <= n; count++)
 tally = tally + 1;
}

void main() { parbegin (total(), total()); }
```

Wiemy, że instrukcje arytmetyczne wykonują się co najmniej dwóch krokach, najpierw zmienna musi zostać załadowana do rejestru i dopiero wykonane jest dodawanie.

W kodzie występuje sytuacja wyścigu, my mamy za zadanie określić z jakiego przedziału mogą pochodzić wartości `tally` (zapewne po zakończeniu wykonywania tych wątków).

### Wartość minimalna

Sprawdźmy kilka pierwszych wartości. Na początek można odrzucić wszystkie wartości ujemne dla `tally` (chyba oczywiste).

Nie może to być `0`, ponieważ oznaczałoby to, że ostatnia (pięćdziesiąta) iteracja wątku który wykonał zapis jako ostatni, zapisał tam 0, ale to niemożliwe, gdyż wczytał jakąś nieujemną wartość i zwiększył ją o 1.

Podobnie nie może to być 1, gdyż wtedy oznaczałoby to, że ostatnia iteracja ostatniego zapisującego coś wątku wczytała 0, a to niemożliwe gdyż 
  * wcześniej zwiększała wartość `tally`, które początkowo wynosi 0 (zatem sama sobie tego zera nie wygenerowała)
  * jedynymi wątkami które mogły podmienić w międzyczasie tą wartość, są też wątki wykonujące `total`, a one potrafią edytować `tally` tylko operacją inkrementacji, czyli wpiszą tam co najmniej 1 (bo przeczytają coś nieujemnego).

Czy może to być 2? Okazuje się, że tak, przypadek będzie wynikał z drugiego punktu uzasadnienia dlaczego nie jest to 1.

Rozważmy taki przeplot dla dwóch wątków:

* Wątek `A` pobiera wartość `tally = 0` i zostaje wywłaszczony
* Wątek `B` rozpoczyna działanie i w spokoju wykonuje 49 iteracji pobrań i inkrementacji.
* Wątek `A` zmartwychwstaje, wykonuje inkrementację, zapisuje `1` i znów zostaje wywłaszczony.
* Wątek `B` budzi się żeby wykonać ostatnią iterację, pobiera `tally = 1` i w tym momencie wraca `A`
* Wątek `A` w wielkim stylu wykonuje resztę swoich iteracji, i kończy działanie
* Wątek `B` zwiększa wartość którą pobrał (`1`) i zapisuje ją do `tally` i kończy działanie

Czyli po zakończeniu `tally = 2`.

Przy `k` wątków sytuacja się nie zmieni. Dalej można uzyskać przeplot dający `2` np. jeśli za `A` przyjmiemy pierwszy wątek. Wątki `2` do `k-1` wykonają się w całości między pierwszą a drugą kropką, a `B` będzie `k`-tym wątkiem.


### Wartość maksymalna

To ten prostszy przypadek, skoro każdy wątek jest w stanie wykonać inkrementację `n = 50` razy, to cudów nie ma, nie da się skończyć z wartością większą niż `n * k`, gdzie `k` to liczba wątków wykonujących `total`. Zetem dla `k=2` będzie to `100`. Przykładowym przeplotem jest po prostu wykonanie się tych wątków jeden po drugim.

## Zadanie 4

### Odpowiednik fork

Do tworzenia nowych wątków będziemy używać procedury`pthread_create(3)`, który przyjmuje 4 argumenty:

* `pthread_t *thread` -- bufor w którym zostanie umieszczony identyfikator utworzonego wątku, jeśli uda się go utworzyć. Identyfikator będzie potrzebny żeby używać innych procedur `pthread` na utworzonym wątku.
* `const pthread_attr_t *attrq` -- wskaźnik na strukturę typu `pthread_attr_t`, zawierającą opis właściwości wątku używanych przy jego tworzeniu (rozmiar stosu, priorytet itd.)
* `void *(*start_routine) (void *)` -- wskaźnik na procedurę, którą ma zacząć wykonywać wątek
* `void *arg` -- wskaźnik na argument, który ma zostać przekazany do `start_routine`

Nowy wątek dziedziczy po wątku wołającym procedurę maskę sygałów blokowanych, ale maska sygnałów oczekujących na obsłużenie jest czyszczona. Tak samo zerowany jest zegar (CPU time clock).


### Odpowiednik exit

Do zakończenia działania wątku można użyć `pthread_exit(3)`, które przyjmuje jeden argument:

* `void *retval` -- wskaźnik na zmienną która przechowuje wartość zwracaną przez wątek. Nie powinnien to być wskaźnik do zmiennej zaalokowanej na stosie wątku, bo zawartość stosu jest niezdefiniowana po zakończeniu jego działania.

Wywołanie to jest równoważne wykonaniu `return` z głównej procedury wątku. Dodatkowo jeśli zawoła ją główny wątek, zamiast zwykłego `exit(3)` to pozostałe wątki dalej moga kontynuować działanie.

### Odpowiednik waitpid

Do czekania na zakończenie wątku odbywa się za pomocą procedury `pthread_join`, przyjmujuje dwa argumenty:

* `pthread_t thread` -- identyfikator wątku, na którego czekamy
* `void **retval` -- bufor w którym umieszczona zostanie wartość zwrócona przez wątek za pomocą `pthread_exit` (NULL jeśli zakończył się w inny sposób, np. przez `pthread_cancel`).

Żeby móc to wykonać wątek musi być w stanie `joinable`, nie musieliśmy być tym wątkiem który go utworzył.

Dołączenie do wątku sprząta zaalokowane do niego zasoby. 
Nie można dołączyć do już pogrzebanego wątku.
Jeśli wiele wątków chce dołączyć do tego samego wątku, to jest to niezdefiniowane zachowanie.

### Odpowiednik atexit

Normalna procedura `atexit(3)` definiuje jaka procedura ma zostać zawołana przy zakończeniu działania procesu.

Zdaje się że nie ma dosłownego odpowiednika dla wątków, można radzić sobie z tym parą makr

```
void pthread_cleanup_push(void (*routine)(void *), void *arg);
void pthread_cleanup_pop(int execute);
```

Pierwsze makro umieszcza na stosie procedur sprzątających podaną procedurę z jej argumentem, a drugie ją ściąga i ewentualnie wykonuje. Głównym problemem takiego podejścia jest, że procedury na tym stosie nie są wykonywane przy `return`, za to są przy `pthread_cancel` i `pthread_exit`. Druga sprawa to że na linuksie te makra są zaimplementowane z użyciem klamer `{...}` dlatego muszą występować w parze i `pthread_exit` musielibyśmy zawołać gdzieś pomiędzy.

Drugie rozwiazanie podpatrzone z: https://lifecs.likai.org/2010/06/pthread-atexit.html

Można użyć `int pthread_key_create(pthread_key_t *key, void (*destructor)(void*))`, żeby stworzyć sztuczny klucz a następnie zapisać pod niego coś niezerowego, spowoduje to, że przy wyjściu z wątku uruchomiona zostanie procedura zdefiniowana przez `destructor`

### Odpowiednik abort

Odpowiednikiem `abort(3)`, który kończy proces w nadzwyczajny sposób jest `pthread_cancel(3)`, który jako argument przyjmuje

* `pthread_t thread` -- identyfikator wątku który ma być zakończony

Jeśli anulowanie wątku jest możliwe (wątek może decydować czy się na to godzi), to wołane są procedury sprzątające oraz wątek zostaje zakończony ze statusem `PTHREAD_CANCELED`

### Wątki złączalne a odczepione

Wątki złączalne (ang. *joinable*) muszą zostać pogrzebany przez inny wątek. Po zakończeniu działania nie zwalnia zasobów, czeka aż inny wątek to zrobi i odczyta jego status. Zwalnianie jest widoczne w implementacji procedury `pthread_join`:

![](https://i.imgur.com/8a41imm.png)

Natomiast w wątka odczepionych (ang. *detached*) zwalnianie zasobów dzieje się automatycznie przy zawołaniu `pthread_exit`. Powoduje to jednak, że inne wątki nie mogą na niego czekać, ani go zabić. Zwalnianie w tym przypadku jest widoczne w implementacji procedury `pthread_exit`:

![](https://i.imgur.com/PToLuw1.png)

## Zadanie 5

Co nieoczekiwanego może się zdarzyć, gdy w środowisku, w którym działa wiele wątków:

### Wykonamy forka

Przy wykonaniu forka do dziecka przekazywany jest wyłącznie wątek go wołający, jednak cały stan obiektów `pthread` (czyli np. mutexów) jest kopiowany wraz z przestrzenią adresową, co sprawia że dziecko przed zawołaniem `execve` może wołać jedynie procedury `async-signal-safe` (bo np. jakiś mutex procedury, którą chcemy zawołać mógł być opróżniony przez niedokończony wątek rodzica).

W oparciu o mana:
![](https://i.imgur.com/d2ZK7xT.png)

### Wykonamy execve

W tym przypadku w przeciwieństwie do forka, wymieniana jest przestrzeń adresowa, więc stany obiektów `pthread` nie będą sprawiały problemów. Jednak przekazywane są wszystkie otwarte deskryptory plików, a wcześniejszy `fork` omijał procedury czyszczące wątków, zatem ciężko zabezpieczyć się przed ich wyciekiem do nowego procesu (trzeba używać `FD_CLOEXEC`).
 
W oparciu o mana:

![](https://i.imgur.com/b2Kbtlr.png)

### Wykonamy exit_group

Procedura ta jest odpowiednikiem `_exit(2)`, tylko zamiast kończyć tylko wątek który ją woła, kończy też wszystkie inne. Zatem nie jest to zamiennik `exit`, który opróżniłby bufory i uruchomił zarejestrowane procedury `atexit`, więc część niewypisanych danych może zostać utracona. 

### Sygnały vs Wątki

Na podstawie `man 7 signal`:

W środowisku wielowątkowym zarejestrowana procedura obsługi sygnału jest wspólna dla wszystkich wątków.

Sam wątek może mieć jednak swoją własną maskę blokowanych sygnałów ustawioną za pomocą `pthread_sigmask`.

Wyróżniamy sygnały `process-directed`, czyli asynchorniczne sygnały pochodzące np. z wywołań `kill(2)`, lub wygenerowane z innego powodu, oraz sygnały `thread-directed`, które np. zostały wygenerowane synchronicznie przez instrukcje wątku, albo zostały celowo wysłane w kierunku wątku np. przez `pthread_kill`.

#### Otrzymanie SIGINT

Jest to sygnał asynchroniczny, więc wpada w kategorię `process-directed` i zostanie obsłużony w kontekście wątku, w którym nie będzie blokowany, jeśli jest więcej niż jeden taki, to wybór będzie losowy:

![](https://i.imgur.com/85O8kuf.png)

#### Otrzymanie SIGPIPE

Jest to sygnał synchroniczny wywołany przez instrukcję wątku, więc wpada w kategorię `thread-directed`, zatem zostanie obsługa sygnału zostanie wykonanan w kontekście wątku który go wywołał.

### Czytanie w kilku wątkach spod tego samego deskryptora plików

Wg. standardu POSIX, wywołania `write` są w stosunku do siebie atomowe jeśli operujemy na plikach zwyczajnych.

![](https://i.imgur.com/ks1xTFF.png)

Więc jedynym problemem mógłby być fakt że nie mamy kontroli nad tym jak będą się przeplatały te wywołania.

## Zadanie 7

Będziemy potrzebować procedury tworzącej wątek, użyjemy `Pthread_create(3)`, który przyjmuje 4 argumenty:

* `pthread_t *thread` -- bufor w którym zostanie umieszczony identyfikator utworzonego wątku, jeśli uda się go utworzyć. Identyfikator będzie potrzebny żeby używać innych procedur `pthread` na utworzonym wątku.
* `const pthread_attr_t *attrq` -- wskaźnik na strukturę typu `pthread_attr_t`, zawierającą opis właściwości wątku używanych przy jego tworzeniu (rozmiar stosu, priorytet itd.)
* `void *(*start_routine) (void *)` -- wskaźnik na procedurę, którą ma zacząć wykonywać wątek
* `void *arg` -- wskaźnik na argument, który ma zostać przekazany do `start_routine`

Oprócz niej, będzie potrzebna procedura, którą będziemy mogli poczekać na zakończenie działających wątków w głównym wątku. Użyjemy `Pthread_join(3)`. Przyjmuje ona dwa argumenty:

* `pthread_t thread` -- identyfikator wątku, na którego czekamy
* `void **retval` -- bufor w którym umieszczona zostanie wartość zwrócona przez wątek za pomocą `pthread_exit`.

### Uzupełniony kod echoclient-thread

```c=
  /* TODO: Start threads and wait for them to finish. */
  pthread_t threads[nthreads];

  for (int i = 0; i < nthreads; i++) {
    Pthread_create( &threads[i], NULL, thread, NULL );
  }

  for (int i = 0; i < nthreads; i++) {
    Pthread_join( threads[i], NULL );
    printf("Thread %d reaped\n", i);
  }
```

### Test

W pliku `echoserver-select` mamy dostarczoną przykładową implementację serwera z wielobieżnością, można jej użyć do połączenia klienta.

```
./echoserver-select 7777 
```

```
./echoclient-thread echoclient-thread.c 10 127.0.0.1 7777
```

Wydruk na serwerze:

```
[...]
Server received 34 (117207 total) bytes on fd 7
Server received 28 (117235 total) bytes on fd 9
Server received 14 (117249 total) bytes on fd 13
Server received 44 (117293 total) bytes on fd 7
Server received 2 (117295 total) bytes on fd 9
Server received 1 (117296 total) bytes on fd 9
Server received 14 (117310 total) bytes on fd 12
Server received 18 (117328 total) bytes on fd 10
Server received 37 (117365 total) bytes on fd 5
Server received 20 (117385 total) bytes on fd 4
Server received 20 (117405 total) bytes on fd 12
Server received 8 (117413 total) bytes on fd 7
Server received 18 (117431 total) bytes on fd 11
Server received 1 (117432 total) bytes on fd 5
Server received 6 (117438 total) bytes on fd 8
Server received 55 (117493 total) bytes on fd 10
Server received 1 (117494 total) bytes on fd 7
Server received 57 (117551 total) bytes on fd 6
Server received 29 (117580 total) bytes on fd 10
Server received 2 (117582 total) bytes on fd 6
Server received 1 (117583 total) bytes on fd 13
Server received 41 (117624 total) bytes on fd 6
Server received 6 (117630 total) bytes on fd 6
Server received 15 (117645 total) bytes on fd 9
Server received 38 (117683 total) bytes on fd 9
[...]
```

Wydruk w kliencie po wysłaniu `SIGINT`:

```
^CQuit requested!
Thread 0 reaped
Thread 1 reaped
Thread 2 reaped
Thread 3 reaped
Thread 4 reaped
Thread 5 reaped
Thread 6 reaped
Thread 7 reaped
Thread 8 reaped
Thread 9 reaped
```

### Czemu nie można go łatwo przerobić na wzór procesów i wait?

??? Nie rozumiem pytania ???

Może chodzi o to:

![](https://i.imgur.com/I3R89U2.png)

To jest z kolei zapewne spowodowane tym, że przy zakończeniu działania wątku nie jest wysyłany sygnał, lub dlatego, że nie ma hierarchicznej struktury w wątkach, więc czekanie na jakikolwiek wątek mogłoby być bez sensu (szczególnie, że poczekanie na już pogrzebany wątek to niezdefiniowane zachowanie).

## Zadanie 8

```c=
int main(int argc, char **argv) {
  if (argc != 2)
    app_error("usage: %s <port>\n", argv[0]);

  Signal(SIGINT, sigint_handler);

  int listenfd = Open_listenfd(argv[1], LISTENQ);
  initclients(listenfd);

  while (!quit) {
    int nready = Poll(fds, nfds, 500);
    if (nready == 0)
      continue;

    /* TODO: If listening descriptor ready, add new client to the pool. */
    if (fds[0].revents & POLLIN) {
      socklen_t clientlen = sizeof(struct sockaddr_storage);
      struct sockaddr_storage clientaddr;
      char client_hostname[HOST_NAME_MAX], client_port[MAXLINE];
      int connfd = Accept(listenfd, (SA *)&clientaddr, &clientlen);
      Getnameinfo((SA *)&clientaddr, clientlen, client_hostname, HOST_NAME_MAX,
                  client_port, MAXLINE, 0);
      addclient(connfd, client_hostname, client_port);
      nready--;
    }

    /* TODO: Echo a text line from each ready connected descriptor.
     * Delete a client when end-of-file condition was detected on socket. */
    int i = 1;
    int byte_num;
    while (nready > 0) {

      if( fds[i].revents & POLLIN ) {
        if( (byte_num = clientread(i)) == 0) {
          delclient(i);
          i--;
        }
        nready--;
      }
      i++;

    }
  }

  printf("Server received %ld total bytes.\n", nbytes);
  return EXIT_SUCCESS;
}
```
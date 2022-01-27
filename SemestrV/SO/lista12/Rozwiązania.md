# Systemy Operacyjne -- Lista 12
###### tags: `SO`

## Zadanie 1

![](https://i.imgur.com/pGfROdc.png)

### Kod programu

```c=
__thread long myid;
static char **strtab;

void *thread(void *vargp) {
  myid = *(long *)vargp;
  static int cnt = 0;
  printf("[%ld]: %s (cnt=%d)\n", myid, strtab[myid], ++cnt);
  return NULL;
}


int main(int argc, char *argv[]) {
  //...
  strtab = argv;
  while (argc > 0) {
    myid = --argc;
    pthread_create(&tid, NULL, thread, (void *)&myid);
  }
  //...
}
```

### Zmienne współdzielone

W sprawie `__thread`: https://gcc.gnu.org/onlinedocs/gcc/Thread-Local.html

| Zmienna | Instancja | Czy współdzielona |
| ------- | --------- | ----------------- |
| myid    | myid.m    | Tak, przekazujemy do niej wskaźnik do wątków tworzonych przez wątek główny. |
| myid    | myid.p[n] | Nie |
| strtab  | strtab    | Tak, odniesienia w każdym wątku |
| vargp   | vargp.p[n]| Nie |
| cnt     | cnt       | Tak, odwoływać się mogą do niej wszystkie wątki tworzone przez wątek główny |
| argc    | argc.m    | Nie |
| argv    | argv.m    | Tak, odwołania są z wątków tworzonych przez główny wątek poprzez wskaźnik `strtab` |

### Zmienne, które są źródłem wyścigów

| Instancja zmiennej | Powód |
| ------- | ----- |
| cnt     | istnieją przeploty, w których dostępy, modyfikacje i zapisy tej zmiennej (z różnych wątków) nie następują sekwencyjnie, więc nie mamy gwarancji, że jej wartość będzie poprawna (tak jak na wykładzie) |
| myid.m    | nie wiemy co wydarzy się pierwsze, czy główny wątek zmniejszy wartość `myid.m`, czy wątki potomne przypiszą jej wartość do własnej zmiennej lokalnej `myid.p[n]` |

## Zadanie 2

![](https://i.imgur.com/Alci7LN.png)

**Sekcja krytyczna** -- segment kodu, który może być wykonywany tylko przez jeden wątek na raz.

W sekcjach krytycznych zazwyczaj modyfikuje się współdzielone zasoby i stosowane są one w celach synchronizacyjnych.

### Problem sekcji krytycznej

Problem polega na zaprojektowaniu protokołu, który wątki mogą wykorzystywać do synchronizacji. Typowo wątki będą przez jakiś czas wykonywać kod leżący poza sekcją krytyczną, prosić o możliwość wejścia do sekcji krytycznej, wykonywać ją, a następnie wracać do kodu leżącego poza nią.

#### Założenia jakie musi spełnić rozwiązanie

* Żadne dwa wątki nie mogą wykonywać na raz tej samej sekcji krytycznej.
* Wątek może być blokowany przed wejściem do sekcji krytycznej tylko przez inne wątki ją wykonujące, lub przez wątki, które również chcą ją wykonywać.
* Oczekiwanie na pozwolenie na wejście do sekcji krytycznej nie może być nieskończone. To znaczy, że musi istnieć limit na liczbę wykonań sekcji krytycznej przez inne wątki po tym jak dany proces poprosił o możliwość jej wykonania.
* Nie można zakładać niczego o względnej prędkości żadnych dwóch procesów.

### Wyłączenie przerwań jako implementacja

Źródło:
OSTEP 28.5

Idea polega to na wyłączeniu wszystkich przerwań przed wejściem do sekcji krytycznej i włączeniu ich przed wyjściem, co uniemożliwiłoby systemowi wywłaszczenie wątku w trakcie jej wykonwyania. 

Problem z tym podejściem polega na tym, że użytkownik z taką mocą mógłby łatwo zawiesić system na stałe, jeśli z jakiegoś powodu przywrócenie przerwań, by nie nastąpiło (np. przez błąd wywołany przez kod sekcji), lub na dłuższą chwilę jeśli wykonanie sekcji krytycznej byłoby długie.

Poza tym to rozwiązanie jest niemożliwe do osiągnięcia w środowisku, w którym jest wiele procesorów.

### Dlaczego blokowanie drobnoziarniste?

***Przypomnienie:*** **Prawo Amdahla** mówi nam, że możliwe do uzyskania przyspieszenie przez zrównoleglanie, jest ograniczone z góry przez część programu, która musi zostać wykonana sekwencyjnie.

![](https://i.imgur.com/kN8ujvn.png)
[grafika pochodzi prawdopodobnie z wykładu Programowanie Współbieżne]

Skoro ścieżki krytyczne muszą być wykonywane sekwencyjnie, to ograniczając ich długość, a zatem ich udział w programie, pozwalamy uzyskać większe przyspieszenie w systemie, który umożliwia obliczenia równoległe.

## Zadanie 3

![](https://i.imgur.com/TIKbNVC.png)

### Compare-and-swap

Źródło: OSTEP 28.9

Instrukcja aktualizuje wartość komórki pamięci na nową, jeśli jej obecna wartość jest taka jak oczekiwana. W przeciwnym przypadku nie robi nic.

```c=
int compare_and_swap(int *ptr, int expected, int new) {
  int actual = *ptr;
  if (actual == expected) {
    *ptr = new;
  }
  return actual;
}
```

### Implementacja blokady wirującej

Źródło: OSTEP 28.7, 28.9

`0` oznacza, że blokada jest zwolniona, a `1` że zajęta.

```c=
typedef int spin_t;

void lock(spin_t *lock) {
  while(compare_and_swap(lock, 0, 1) == 1)
    ;
}

void unlock(spin_t *lock) {
  *lock = 0;
}

```

#### Dlaczego nie jest sprawiedliwa?

Źródło: OSTEP 28.4, 28.8

Jednym z kryteriów oceny blokady jest *sprawiedliwość* (ang. *fairness*), czyli ocena czy blokada pozwala wykonywać się każdemu z wątków. Czyli np. czy nie powoduje zagłodzenia wątków o niższym priorytecie, poprzez niedopuszczanie ich do możliwości wykonywania ścieżki krytyczniej.

Nasza blokada oczywiście nie daje żadnych gwarancji co do tego, że jakiś  wątek nie utknie na pętli `while` w `lock`, bo będzie zawsze ubiegany przez inny wątek w sprawdzeniu nowej wartości blokady po jej zwolnieniu.

Wątki mogą być zagłodzone przy blokadach wirujących.

### Scenariusz

* `n` wątków chce wykonać ścieżkę krytyczną
* wątki są wywłaszczane przez planistę wg. algorytmu *round-robin*, w którym kwant czasu to 1ms.

#### Algorytm planisty

*Round-robin* to bardzo prosty algorytm planisty. Dla każdego wątku z uporządkowanej listy cyklicznej wszystkich aktywnych wątków:

* Pozwóla pracować mu dokładnie przez kwant czasu, chyba że zakończy się wcześniej.
* Wywłaszcza go.
* Przechodzi do kolejnego wątku z listy.

Zatem wszystkie wątki mają taki sam priorytet oraz każdy będzie miał okazję żeby się wykonać, co sprawia, że algorytm ten nigdy nie zagłodzi wątków.

![](https://i.imgur.com/UGQhqlA.png)
[Autor ilustracji: 	Ben Meiri, źródło: https://commons.wikimedia.org/wiki/File:Round_Robin_Schedule_Example.jpg]

#### Ile czasu zajmie wątkom przejście przez ścieżkę krytyczną?

Założenia:
* Te `n` wątków to jedyne wątki w systemie.
* Kod ścieżki wykonany sekwencyjnie wykonuje się równo `tc` milisekund.
* Użyte blokady to blokady wirujące.
* Pomijamy czas potrzebny na obsługę wywłaszczania (ale pamiętamy, że to niezerowy koszt).

##### Schemat działania

Pierwszy wątek:

* Pierwszy wątek poprosi o wejście do ścieżki krytycznej, uda mu się to.
* Wykona jej pierwsze 1ms i zostanie wywłaszczony.
* Każdy z pozostałych `n-1` wątków pokręci się na pętli `while` w `lock` przez 1ms.
* Pierwszy wątek wykona kolejną 1ms z pętli krytycznej.
* Schemat powtórza się aż pierwszy wątek nie wykona ostatniej milisekundy sekcji krytycznej, wtedy zgodę otrzyma drugi wątek.

W skrócie:

$$
  t_1 = (t_c - 1) \cdot (1 + n - 1) + 1 = (t_c - 1) \cdot  n + 1
$$

Pozostałe wątki:

Dla pozostałych wątków sytuacja będzie wyglądać podobnie, z tym że zmniejszać się będzie liczba kręcących się w pętli wątków.

$$
  t_k = (t_c - 1) \cdot (1 + n - k) + 1
$$

Podsumowując:

$$
  t = \sum_{k = 1}^{n} ((t_c - 1)(1 + n - k) + 1) = n + \sum_{k = 1}^{n} (t_c - 1)(1 + n - k) = \\
  = n + \sum_{k = 0}^{n - 1} (t_c - 1)(1 + k)
$$

## Zadanie 4

![](https://i.imgur.com/0OHxN7F.png)

W poprzednim zadaniu widzieliśmy blokadę polegającą na idei **aktywnego czekania**, czyli takiego, w którym faktycznie wykorzystujemy przydzielony nam czas procesora wyłącznie na czekanie na jakieś zdarzenie.

### Yield

Źródło: OSTEP 28.13

Alternatywne podejście wymaga wsparcia nie tylko ze strony sprzętu, ale też systemu. Załóżmy, że udostępniono nam wywołanie `yield`, którym dobrowolnie możemy zrzec się przydzielonego nam czasu procesora ("dobrowolne wywłaszczenie"). Teraz można przerobić procedurę `lock` z poprzedniego zadania:

```c=
void lock(spin_t *lock) {
  while(compare_and_swap(lock, 0, 1) == 1)
    yield();
}
```

Pomoże to uniknąć marnowania czasu na kręcenie się w pętli.

#### Dlaczego nie unikniemy problemów blokad wirujących?

Rozpatrzmy scenariusz z poprzedniego zadania dla jakiegoś bardzo dużego `n` (np. 100). 

Teraz wywłaszczenie wątku nr. 1 kiedy jest on w środku wykonywania sekcji krytycznej powoduje 99 sprawdzeń warunku w pętli i yieldowania przez inne aktywne wątki na zmianę, a pamiętamy że koszt obsługi wywołań systemowych potrafi być zauważalny kiedy jest ich dużo. Dodatkowo nie można zapomnieć, że wywłaszczenie i zmiana kontekstu też nie jest za darmo.

Zatem, mimo że to rozwiązanie nie spala całych kwantów czasu na czekanie, to problem marnowania zasobów na czekanie wciąż istnieje.

Problem sprawiedliwości blokady nie został w żaden sposób zaadresowany i również wciąż istnieje.

### Blokady usypiające

Idea polega na zwiększeniu kontroli nad tym, które wątki są uruchamiane po zwolnieniu danego zasobu.

#### Implementacja

Źródło: OSTEP 28.14

Potrzebna będzie kolejna dawka wsparcia ze strony systemu operacyjnego. Tym razem załóżmy, że mamy wywołanie usypiające wątek je wołający (`park()` w solarise) oraz wywołanie wybudzające wątek o konkretnym identyfikatorze (`unpark(threadID)` w solarisie).

Idea tej implementacji polega na utrzymywaniu kolejki wątków, które czekają na zwolnienie danej blokady. Dostęp do tej kolejki oraz flagi wskazującej, że blokada jest zajęta będzie chroniony blokadą wirującą, co nie jest problemem (wytłumaczenie później).

W poniższej implementacji będziemy potrzebować jeszcze wywołania `setpark()`, które informuje system, że wątek zaraz chce pójść spać i jeśli zanim to zrobi w innym wątku zostanie zawołane `unpark`, to ma tego wątku nie uspiać przy najbliższym `park()` (szczegóły później).

```c=
// Struktura blokady
typedef struct __lock_t {
  int flag;   // - mówi czy blokada jest zajęta przez wątek
  int guard;  // - chroni dostęp do tej struktury 
              // (tylko jeden wątek może ją edytować na raz)
  queue_t *q; // - kolejka identyfikatorów wątków, które czekają na
              // zwolnienie tej blokady
} lock_t;


void lock_init(lock_t *m) {
  m->flag = 0;
  m->guard = 0;
  queue_init(m->q);
}

void lock(lock_t *m) {
  // prosimy o blokadę chroniącą strukturę blokady
  while (TestAndSet(&m->guard, 1) == 1)
    ;
  
  if (m->flag == 0) {
    // blokada jest zwolniona
    m->flag = 1;  // zajmujemy ją
    m->guard = 0; // zwalniamy blokadę chroniącą strukturę
  } else {
    // blokada jest zajęta przez inny wątek
    queue_add(m->q, gettid()); // dodajemy ten wątek do 
                               // kolejki wątków oczekujących
    setpark();    // informujemy że chcemy iść spać
    m->guard = 0; // zwalniamy blokadę chroniącą strukturę
    park();       // idziemy spać, jeśli zostaniemy wybudzeni,
                  // to będzie to oznaczało że otrzymaliśmy
                  // blokadę od wątku ją przetrzymującego
  }
}

void unlock(lock_t *m) {
  // prosimy o blokadę chroniącą strukturę blokady
  while (TestAndSet(&m->guard, 1) == 1)
    ; 
  
  if (queue_empty(m->q))
    // Nikt nie czeka na zwolneinie tej blokady
    m->flag = 0; // zwalniamy blokadę
  else
    // Co najmniej jeden wątek czeka na zwolnienie blokady
    unpark(queue_remove(m->q)); // wybudzamy wątek oczekujący 
  m->guard = 0; // zwalniamy blokadę chroniącą strukturę
}
```

##### Dlaczego blokady wirujące nas teraz nie bolą?

Wciąż może wystąpić sytuacja z trzeciego zadania, w której wątek przetrzymujący blokadę na modyfikację struktury zostanie wywłaszczony, a inne będą musiały się pokręcić na pętli o nią prosząc, ale liczba razy ile to się może zdarzyć jest ograniczona przez długości krótkich sekcji krytycznych w procedurach `lock` i `unlock`, a nie przez potencjalnie długie sekcje krytyczne definiowane przez użytkownika.

##### Po co jest setpark?

Załóżmy, że go nie ma.

```c=
void lock(lock_t *m) {
  // [...]
  } else {
    // blokada jest zajęta przez inny wątek
    queue_add(m->q, gettid());
    m->guard = 0;
    park();
  }
}
```

Zwróćmy uwagę, że istnieje teraz krótkie okno pomiędzy instrukcją `m->guard = 0`, a `park()`, w którym wątek może zostać wywłaszczony, kontrolę może dostać wątek dzierżący blokadę, następnie ten wątek może zwolnić blokadę, spojrzeć na kolejkę, wybudzić ten wątek który jeszcze nie poprosił o uśpienie, a ten z kolei pierwsze co zrobi to właśnie wykona `park()` i zaśnie na wieki, amen.

Warto też zauważyć, że zamiana kolejności tych instrukcji nie wchodzi w grę.

## Zadanie 5

![](https://i.imgur.com/vXYiVz5.png)

### Warunki konieczne do wystąpienia zakleszczenia

Są to tzw. warunki Coffmana:

* *Mutual-Exlusion* -- Muszą istnieć zasoby, które są współdzielone przez wątki i tylko jeden wątek na raz może z nich korzystać. 
* *Hold and wait* -- Muszą istnieć procesy, które jednocześnie przetrzymują zasób konieczny do swojego wykonania oraz proszą o dostęp do kolejnych zasobów.
* *Non preemption* -- Zasób może zostać zwolniony tylko dobrowolnie przez wątek go przetrzymujący.
* *Circular wait* -- Może zaistnieć cykl przy próbach pozyskania zasobów, w którym każde ogniwo posiada zasób potrzebny do działania wcześniejszego oraz prosi o zasób przetrzymywany przez kolejne.

### Jak przeciwdziałać zakleszczeniom?

Źródło: OSTEP 32.3 podrozdział Prevention

#### Atak na warunek circular wait

Okazuje się, że można łatwo uniknąć cyklicznych zależności jeśli ustalimy, że blokady na zasoby moga być pozyskiwane tylko w ściśle określonej kolejności.

##### Dowód

Weźmy graf skierowany $G$, w którym wierzchołkami są wątki i zasoby. Krawędź od zasobu $x$ do wątku $A$ oznacza, że wątek $A$ jest w posiadaniu zasobu $x$. Krawędź od wątku $B$ do zasobu $y$ oznacza, że wątek $B$ chce pozyskać zasób $y$.

Jeśli w takim grafie możliwe jest uzyskanie cyklu, to może zaistnieć zakleszczenie. Pokażemy, że przy określeniu kolejności pobierania zasobów, jest to niemożliwe.

Każdemu zasobowi przyporządkowujemy unikalną wartość odpowiadającą jego pozycji w kolejności pobierania blokad. Wartość wątku natomiast będzie równa największej wartości wątku, który posiada.

Teraz nasze ograniczenie można sformułować inaczej: wątek może pożądać tylko zasoby o większej wartości od jego własnej.

**Załóżmy nie wprost**: W grafie $G$ istnieje skierowany cykl $C$.

Weźmy wierzchołek z cyklu $C$ o minimalnej wartości. Mogą istnieć takie dwa (jeden wątek, a drugi zasób), ale nie więcej (bo oznaczałoby to, że dwa wątki posiadają jeden zasób, lub są dwa zasoby o tej samej wartości). Wybieramy ten leżący na cyklu wcześniej, nazwijmy go $v_i$.

Istnieje jakiś wierzchołek $v_j$ na niego wskazujący. Z tego że $value(v_i)$ jest minimalne, oraz że z możliwych dwóch minimalnych leży on na ścieżce wcześniej możemy wywnioskować, że

$$
value(v_j) > value(v_i)
$$

Rozpatrzmy przypadki:

* $v_j$ to zasób -- wtedy $v_i$ to wątek i mamy sprzeczność, bo wartość wątku to wartość najbardziej wartościowego zasobu. 
* $v_j$ to wątek -- wtedy $v_i$ to zasób i mamy sprzeczność, bo wątek pożąda zasobu o mniejszej wartości niż jego wartość.

W obu przypadkach sprzeczność, czyli w grafie $G$ nie ma cykli.

#### Atak na warunek hold and wait

Sposobem na nigdy nie wystąpienie tego warunku w systemie jest pozyskiwanie wszystkich zasobów potrzebnych wątkowi na raz i atomowo. Można to uzyskać wprowadzając np. blokadę na pozyskiwanie blokad:

```
lock(prevention);
lock(L1);
lock(L2);
...
unlock(prevention);
```

Nie jest to jednak praktyczny sposób, gdyż bardzo rzadko zdarza nam się wiedzieć z wyprzedzeniem jakie zasoby będą potrzebne, poza tym łamie to omawiane już zasady blokowania drobnoziarnistego.

#### Atak na warunek no preemption

Możemy zaprogramować wątki tak, że jeśli są w posiadaniu jakiegoś zasobu i jednocześnie potrzebują kolejne, to prośba o nie może się udać tylko za pierwszym razem, jeśli się nie uda, to dobrowolnie zwalniamy wszystkie pozyskane blokady i zaczynamy proces od nowa.

Podejście to faktycznie sprawi, że *deadlock* nie wystąpi, ale wprowadzi możliwość wystąpienia omawianego na dziesiątej liście *livelocka*.

#### Atak na warunek mutual exclusion

Możemy (mimo, że nie jest to łatwe) projektować struktury danych i procedury, tak żeby wątki nigdy nie musiały wprost używać blokad do ich modyfikowania, a zamiast tego używały złożonych instrukcji atomowych takich jak np. compare-and-swap.

Dla przykładu w podręczniku podana była implementacja bezpiecznej inkrementacji licznika:

```c=
void AtomicIncrement(int *value, int amount) {
  do {
    int old = *value;
  } while (CompareAndSwap(value, old, old + amount) == 0);
}
```

oraz wstawiania wartości do listy:

```c=
void insert(int value) {
  node_t *n = malloc(sizeof(node_t));
  assert(n != NULL);
  n->value = value;
  do {
    n->next = head;
  } while (CompareAndSwap(&head, n->next, n) == 0);
}
```

Jest to tzw. synchronizacja *wait-free*.

#### Unikanie zakleszczeni dzięki planiście

W systemach, w których wiadomo które zasoby będą pozyskiwane przez które wątki, można zaimplementować planistę, który będzie decydował o tym które wątki mogą się przeplatać w danym momencie i pozwalał im na wykonanie się zgodnie z tym planem.

Przykładem takiego algorytmu jest algorytm bankiera.

#### Wykryj i napraw

W ostateczności możemy zaimplementować jakiegoś rodzaju system wykrywający, że wystąpiło zakleszczenie i próbujący przywrócić stan, w którym go nie było np. przez siłowe wywłaszczanie, lub nawet zabijanie i restart wątków.

### Jak lockdep wykrywa możliwośc zakleszczenia

(Na podstawie  pracy licencjackiej *Dynamic Verification of Concurrency in Operating Systems* Jakuba Urbańczyka)

Narzędzie lockdep śledzi działanie programu i na podstawie wykrytych schematów próśb i przydzielania blokad próbuje wnioskować na temat tego czy istnieje ryzyko wystąpienia zakleszczenia.

Blokady są grupowane w *klasy blokad*, tzn. nie będzie śledzona każda pojedyncza instancja blokady, tylko instancje danej blokady będą śledzone wspólnie.

Idea wykrywania polega na inferowaniu kolejności w jakiej powinny być brane blokady. W tym celu dla każdej klasy $L$ utrzymywane są dwie listy:

* lista $before$ -- czyli lista klas blokad, w których posiadaniu były wątki kiedy otrzymał blokadę klasy $L$. 
* lista $after$ -- czyli lista klas blokad, w których posiadanie wchodziły wątki, kiedy posiadały $L$.

Te dwie listy definiują w jakiej kolejności względem $L$ powinny być pozyskiwane blokady. Jeśli blokada $L$ jest pozyskiwana, to lockdep sprawdzi czy nie występują jakieś sprzeczności w dotychczasowej wyinferowanej kolejności. Jeśli występują, to znaczy że istnieje ryzyko zakleszczenia.

#### Z jakimi scenariuszami sobie nie poradzi?

Program nigdy nie daje fałszywych negatywów, jeśli nie mówi że coś jest źle, to jest dobrze. Może natomiast dawać fałszywe pozytywy jeśli sposoby korzystania z blokad zmieniają się w zależności od stanu, lub gdy blokady nie są zakładane w takiej samej kolejności konsekwentnie w taki sposób, że do zakleszczenia i tak by nie doszło, czego przykładem ze źródła jest:

```c=

struct node_data {
  ...
  mutex_t lock;
};
struct tree_node {
  ...
  struct node_data *data;
  mutex_t lock;
};

void lock_tree_node_and_storage(struct tree_node *A) {
  if (is_root(A)) {
    mutex_lock(A->lock);
    mutex_lock(A->data->lock);
  } else {
    mutex_lock(A->data->lock);
    mutex_lock(A->lock);
  }
}
```

Żaden wierzchołek nie może być jednocześnie korzeniem i nie-korzeniem, więc do zakleszczenia by nie doszło, ale lockdep nie jest w stanie tego zrozumieć i będzie krzyczał, bo kolejność jest zaburzona.

## Zadanie 6

![](https://i.imgur.com/wQjHtMt.png)

### Kod propozycji

```c=
shared boolean blocked [2] = { false, false };
shared int turn = 0;

void P (int id) {
  while (true) {
    blocked[id] = true;
    while (turn != id) {
      while (blocked[1 - id])
        continue;
      turn = id;
    }
    /* put code to execute in critical section here */
    blocked[id] = false;
  }
}

void main() { parbegin (P(0), P(1)); }
```

### Idea propozycji

* W tablicy `blocked` trzymamy flagi mówiące czy wątki obecnie wykonują segment kodu nie związany z blokadą (false), czy wykonują ścieżkę krytyczną lub zwalniają / proszą o blokadę (true).
* W zmiennej `turn` trzymamy informację o tym, który wątek obecnie powinien wykonywać sekcję krytyczną.
* Każdy wątek przy pobraniu blokady:
  * Sprawdza czy jest jego kolei.
  * Jeśli tak, to przechodzi do ścieżki krytycznej
  * Jeśli nie, to czeka aż drugi wątek skończy sekcję krytyczną, kiedy to się stanie, to próbuje ustawić `turn` na swoje `id`, jeśli mu się uda to wykonuje ścieżkę krytyczną.
  * Po zakończeniu sekcji krytycznej, ustawia flagę `blocked` na `false` i wykonuje resztę kodu, jeśli taka jest.

### Kontrprzykład

Istnieje krótkie okno czasowe pomiędzy wyjściem wyjściem z pętli `while (blocked[1 - id])`, a instrukcją `turn = id;`, w której drugi wątek może zacząć wykonywanie sekcji krytycznej, bo zmiana `turn` jeszcze nie nastąpiła:

```
Wątek P(1) startuje pierwszy:

while (true)
blocked[1] = true;
while (0 != 1)
while (blocked[0]) // blocked[0] == false, wychodzi z pętli

---- Wywłaszczenie, teraz P(0) ----
while (true)
blocked[0] = true;
while (0 != 0) // pętla pominięta
* zaczyna wykonywać ścieżkę krytyczną *

---- Wywłaszczenie, teraz kontynuuje P(1) ----
turn = 1;
while (1 != 1) // pętla pominęta
* zaczyna wykonywać ścieżkę krytyczną *

Dwie ścieżki krytyczne wykonują się jednocześnie!

```

## Zadanie 7

![](https://i.imgur.com/pl5UiBK.png)

### Rozważana implementacja

```c=
// blocked -- true ozncza, że wątek jest zainteresowany 
// wejściem do sekcji krytycznej, false wpp.
shared boolean blocked [2] = { false, false }; 
// turn -- oznacza który wątek ma pierwszeństwo
shared int turn = 0;

void P (int id) {
  while (true) {
    blocked[id] = true; // wątek jest zainteresowany sekcją krytyczną
    turn = 1 - id;      // drugi wątek ma pierwszeństwo
    // dopóki inny wątek jest zainteresowany sekcją krytyczną
    // i nie jest obecnie nasza kolej, to się kręcimy
    while (blocked[1 - id] && turn == (1 - id))
      continue;
    /* put code to execute in critical section here */
    blocked[id] = false;
  }
}

void main() { parbegin(P(0), P(1)); }
```

### Dowód poprawności

(Na podstawie podręcznika *The Art of Multiprocessor Programming*)

Problem wzajemnego wykluczenia to inaczej *problem sekcji krytycznej* z zadania 2. Musimy zatem pokazać, że spełnia ono warunki konieczne do rozwiązania tego problemu.

#### Żadne dwa wątki nie moga wykonywać sekcji krytycznej jednocześnie

Załóżmy nie wprost, że dwa wątki o indeksach $0$ i $1$ znalazły się jednocześnie w swoich sekcjach krytycznych $CS_0$ i $CS_1$.

Rozważmy ostatnie instrukcje, które wątki wykonały przed wejściem do ścieżki krytycznej.

Pierwszy wątek:

$write_0(blocked[0] = true) \rightarrow write_0(turn = 1) \rightarrow 
read_0(blocked[1]) \rightarrow read_0(turn)$

Drugi wątek:

$write_1(blocked[1] = true) \rightarrow write_1(turn = 0) \rightarrow 
read_1(blocked[0]) \rightarrow read_1(turn)$

Załóżmy bez straty ogólności, że wątek nr $0$ zapisał zmienną $turn$ jako ostatni. Czyli mamy

$$
write_1(turn = 0) \rightarrow write_0(turn = 1)
$$

Skoro wątek $0$ ustawił tą zmienną jako ostatni, to znaczy, że przeczytał ustawioną przez siebie wartość w warunku pętli.

$$
write_0(turn = 1) \rightarrow read_0(turn == 1)
$$

Wiemy jednak, że wątek wszedł do ścieżki krytycznej, czyli nieprawdziwy musiał być warunek `blocked[1 - id] && turn == (1 - id)`. Skoro `turn == 1 == (1 - id)`, to wątek musiał zaobserować, że `blocked[1] == false`.

$$
write_0(turn = 1) \rightarrow 
read_0(blocked[1] == false) \rightarrow read_0(turn == 1)
$$

Z powyższych można wywnioskować, że

$$
write_1(blocked[1] = true) \rightarrow write_1(turn = 0) \rightarrow write_0(turn = 1) \\ \rightarrow 
read_0(blocked[1] == false)
$$

Czyli 

$$
write_1(blocked[1] = true) \rightarrow
read_0(blocked[1] == false)
$$

Mamy sprzeczność, gdyż zmienna `blocked[1]` nie była modyfikowana w żadnym innym miejscu przed wejściem do ścieżek krytycznych.

#### Wątki nie mogą czekać nieskończenie długo

Załóżmy nie wprost i bez straty ogólności, że to wątek nr. $0$ utknął w którymś miejscu. Jedyną blokującą częścią kodu jest pętla `while`. Rozpatrzmy więc przypadki:

* Wątek $1$ również utknął na tej pętli. Jednak skoro wartość zmiennej $turn$ to albo $0$, albo $1$ to warunek pętli w jednym z wątków musi być fałszywy (inaczej mówiąc zmienna $turn$ nie może mieć dwóch wartości jednocześnie), stąd wnioskujemy sprzeczność.
* Wątek $1$ wychodzi i wchodzi do swojej sekcji krytycznej bez oddania czasu wątkowi $0$. Jednak skoro wątek $0$ utknął na pętli `while`, to `blocked[0] == true`, a wątek $1$ przed każdym ponownym wejściem w sekcję krytyczną ustawia `turn == 0` i powinien utknąć na pętli while, bo `turn` zmieni się dopiero przy ponownym wejściu w `lock` wątku $0$. Mamy sprzeczność.

Jeśli wątki utknęły w innych miejscach, to nie może to być wina algorytmu.

#### Wątek może być blokowany wyłącznie przez wątki wykonujące sekcję krytyczną

Jest tylko jedno miejsce, w którym wątek może się zablokować, czyli pętla `while`. Zablokowanie się w niej może się zdarzyć wyłącznie wtedy kiedy inny wątek jest zainteresowany wykonywaniem sekcji krytycznej (ustawił swoją flagę `blocked` na true).


#### Nie zakładamy niczego o względnej prędkości procesów

To, mam nadzieję, widać. Bazujemy tylko na fakcie czy wątek jest zainteresowany wejściem w sekcję krytyczną i którego wątku obecnie jest kolej.

## Zadanie 8

![](https://i.imgur.com/ERCBrMD.png)

### Rozważana implementacja

```c=
struct csem {
  bsem mutex;
  bsem delay;
  int count;
};

void csem::csem(int v) {
  mutex = 1;
  delay = 0;
  count = v;
}

void csem::P() { // take
  P(mutex);
  count--;
  if (count < 0) {
    V(mutex);
    P(delay);
  } else {
    V(mutex);
  }
}

void csem::V() { // give
  P(mutex);
  count++;
  if (count <= 0)
    V(delay);
  V(mutex);
}
```

### Idea

* pole `mutex` -- chroni ścieżki krytyczne przy modyfikacji zmiennej `count`.
* pole `delay` -- blokada, którą wykorzystuje się przy usypianiu w oczekiwaniu na zwiększenie licznika.
* pole `count` -- jeśli dodatnie, oznacza liczbę dostępnych zasobów do wzięcia, wpp. oznacza liczbę wątków które na nie oczekują.

### Kontrprzykład

Problem jest lustrzanie podobny do tego, który spotkaliśmy kiedy usunęliśmy `setpark` w zadaniu 4.

Kolejne warunki do spowodowania błędu:
* Zaczynamy od pustego semafora `csem sem(0)`
* W wątku nr 1 próbujemy podnieść semafor `P(sem)`:
```c=
P(mutex); // bierzemy blokadę na count, mutex == 0
count--;  // count == -1
if (count < 0) { // -1 < 0
  V(mutex); // zwracamy blokade na count, mutex == 1
  // Tutaj chcielibyśmy isć spać, ale następuje wywłaszczenie
```
* Kontrola przekazana jest do wątku nr 2, który robi dokładnie to samo i również jest wywłaszczany w tym samym miejscu.
```c=
P(mutex); // bierzemy blokadę na count, mutex == 0
count--;  // count == -2
if (count < 0) { // -2 < 0
  V(mutex); // zwracamy blokade na count, mutex == 1
  // Tutaj chcielibyśmy isć spać, ale następuje wywłaszczenie
```
* Teraz kontrolę otrzymuje wątek nr 3, który chce zwiększyć semafor `V(sem)`.
```c=
P(mutex); // bierzemy blokadę na count, mutex == 0
count++;  // count == -1
if (count <= 0) // -1 <= 0
  V(delay);     // "wybudzamy" czekający wątek, ale
                // żaden nie czeka, dlatego, delay == 1
V(mutex); // zwracamy blokade na count, mutex == 1
```
* W następnej kolejności czas procesora dostaje wątek nr 4, który robi to samo co trzeci.
```c=
P(mutex); // bierzemy blokadę na count, mutex == 0
count++;  // count == 0
if (count <= 0) // 0 <= 0
  V(delay);     // "wybudzamy" czekający wątek, ale
                // żaden nie czeka, na dodatek delay == 1, 
                // dlatego pozostanie bez zmian
V(mutex); // zwracamy blokade na count, mutex == 1
```
* Po jakimś czasie do życia wraca wątek 1 (albo 2, nieistotne), wykonuje kolejne instrukcje od miejsca, w którym skończył.
```c=
P(delay) // delay == 1, dlatego nie pójdzie spać (jeszcze nie ma tragedii).
         // Od teraz delay == 0.
[...]
```
* Ostatecznie wykonanie wznawia wątek 2.
```c=
P(delay) // delay == 0, dlatego wątek pójdzie spać, otrzymujemy
         // niepoprawny stan, bo count == 0, a jeden wątek śpi.
```

## Zadanie 9

![](https://i.imgur.com/MBokHWF.png)

### Wadliwy kod

```python=
def producer():
  while True:
    item = produce()
    if queue.full():
      sleep()
    queue.push(item)
    if not queue.empty():
      wakeup(consumer)
      
def consumer():
  while True:
    if queue.empty():
      sleep()
    item = queue.pop()
    if not queue.full():
      wakeup(producer)
    consume(item)
```

### Błędy wykonania

#### queue.push(item)

Zakończy się błędem wykonania na pewno jeśli kolejka będzie w tym miejscu pełna. 

Taki może być efekt następujęcego przeplotu (zaczynamy od pełnej kolejki):
* pierwszy działa `consumer`:
  * kolejka jest niepusta -- pomijamy spanie
  * usuwamy jeden element
  * kolejka nie jest pełna, więc wchodzimy do if'a
  * następuje wywłaszczenie, kolejce brakuje jednego elementu do zapełnienia
* teraz działa `producer`:
  * produkuje element
  * kolejka nie jest pełna, więc pomijamy spanie
  * dodajemy element do kolejki, teraz jest pełna
  * kolejka jest niepusta, budzimy konsumenta
  * produkujemy kolejny element
  * kolejka jest pełna, więc idziemy spać
* `consumer` kontunnuje:
  * budzi producenta 
* `producer` wstaje:
  * dodaje element do (pełnej) kolejki -- błąd

#### item = queue.pop()

Zakończy się błędem wykonania na pewno jeśli kolejka będzie w tym miejscu pusta. 

Taki może być efekt następujęcego przeplotu (zaczynamy od pustej kolejki):
* pierwszy działa `producer`:
  * produkuje element
  * kolejka nie jest pełna -- pomijamy spanie
  * dodajemy jeden element do kolejki
  * kolejka nie jest pusta, więc wchodzimy do if'a
  * następuje wywłaszczenie, w kolejce jest jeden element
* teraz działa `consumer`:
  * kolejka nie jest pusta, więc pomijamy spanie
  * zdejmujemy element z kolejki, teraz jest pusta
  * kolejka nie jest pełna, budzimy producenta
  * konsumujemy element
  * kolejka jest pusta więc idziemy spać
* `producer` kontunnuje:
  * budzi konsumenta 
* `consumer` wstaje:
  * zdejmuje element z (pustej) kolejki -- błąd

### Zakleszczenie

Problem polega na tym, że da się doprowadzić do sytuacji w której oba wątki pójdą spać, mimo że jeden z nich może kontynuować pracę (na tym polega problem *lost wake-up*).

Zaczynamy od pustej kolejki:
* Działa `consumer`:
  * kolejka jest posta, zatem wchodzi do if'a, ale jest wywłaszczony zanim faktycznie wykona `sleep()`.
* Działa `producer`:
  * produkuje element
  * kolejka nie jest pełna, zatem nie idzie spać
  * dodaje element do kolejki
  * kolejka nie jest pusta, więc budzi konsumenta
  * powtarza to aż kolejka się zapełni i sam pójdzie spać
* Konsument kontynuuje
  * Idzie spać mimo, bo to jest kolejna instrukcja która miał wykonać.

Oba wątki poszły spać, mimo że bufor jest pełny. Nikt nikogo już nie obudzi zatem jest to zakleszczenie.
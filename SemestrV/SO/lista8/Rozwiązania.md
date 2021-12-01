# Systemy Operacyjne -- Lista 8
###### tags: `SO`

## Zadanie 1

![](https://i.imgur.com/IMmeNLA.png)


### Jakie są wady stosowania sbrk?

Wywołanie `sbrk` przesuwa `brk` w pamięci wirtualnej procesu, czyli wskaźnik na koniec sterty, w efekcie alokując lub zwalniając pamięć.

Rozważmy program, który alokuje duży blok pamięci, następnie mniejszy, który ląduje bezpośrednio za nim w przestrzeni adresowej.

![](https://i.imgur.com/gcJiabn.png)

Następnie zwalnia duży blok:

![](https://i.imgur.com/vlVuOwS.png)

Teraz mimo że mamy duży ciągły obszar pamięci który jest wolny, to nie możemy go zwrócić do jądra, gdyż żeby to zrobić musielibyśmy cofnąć `brk`, a to niemożliwe, bo powstrzymuje nas przed tym ostatni zajęty blok (nie możemy go przenieść, gdyż użytkownik już dostał do niego wskaźnik).

Tego typu zachowania mogą prowadzić do sytuacji w której mamy dużo rozległych, ciągłych wolnych obszarów pamięci które są blokowane przed zwolnieniem przez mniejsze znajdujące się za nimi.

### Jak można to poprawić używając mmap i munmap?

Żeby zredukować wpływ tego zjawiska, duże obszary pamięci zazywczaj alokuje się z użyciem `mmapa`, ponieważ wtedy przy zawołaniu `free` na takim obszarze, możemy od razu zwrócić go do jądra 

### Kiedy procedura free może zwrócić pamięć do jądra?

Tylko wtedy jeśli zwolniła pamięć znajdującą się bezpośrednio przed `brk`, lub jeśli pamięć zwalniania była zmapowana przez `mmap`.

## Zadanie 2

![](https://i.imgur.com/qy6llEY.png)

### Fragmentacja wewnętrzna

![](https://i.imgur.com/U6O2oBS.png)

Występuje kiedy przydzielamy blok większy niż wynikałoby to z prośby użytkownika. Czyli jego dane (payload) zajmują tylko część zaalokowanego bloku. Użytkownik oczywiście wciąż otrzyma wskaźnik na początek payloadu.

Powstaje często w wyniku wplatania danych wykorzystywanych w strukturach alokatora, np. rozmiaru bloku. Ale także na skutek polityk pozwalających na zwracanie większych bloków, czy też paddingu wymuszonego wymaganiami co do wyrównania bloków, albo minimalnego rozmiaru bloku.

### Fragmentacja zewnętrzna

![](https://i.imgur.com/aEYbD37.png)

Ta z kolei dotyczy przestrzeni pomiędzy przydzielonymi blokami. Występuje kiedy łącznie mamy wystarczająco wolnego miejsca żeby wykonać żądanie, ale nie w pojedynczym ciągłym obszarze pamięci. (w ogólności: kiedy z jakiegoś powodu nie możemy użyć wolnej przestrzeni)

Jest to problem, gdyż nie możemy wykonywać kompaktowania.

#### Dlaczego malloc nie może kompaktować

Kompaktowanie musiałoby polegać na przesunięciu już zarezerwowanych bloków w inne miejsce w pamięci, ale skoro wskaźnik na nie już został zwrócony użytkownikowi, to nie mamy z poziomu biblioteki żadnej kontroli nad tym gdzie znajdują się odwołania do przesuwanego obszaru, które teraz trzeba by  zaktualizować żeby dalej wskazywały na te same dane co przed przesunieciem.

#### Dwie główne przyczyny występowania fragmentacji zewnętrznej

W podanym dokumencie znajdziemy dwie wyróżnone przyczyny:

##### Pierwsza : "Fragmentation is caused by isoletad deaths"

Autorzy dokumentu sugerują, że ważnym czynnikiem są często powstające bloki wolnej pamięci, które sąsiadują z zajętymi. Można z tym walczyć starając się układać blisko siebie bloki, które z dużym prawdopodobieństwem zostaną zwolnione po kolei, bez przeplatania z prośbami o alokację. W ten sposób dzięki złączaniu wolnych bloków, utworzymy znacznie większe bloki ciągłej wolnej pamięci.

##### Druga : "Fragmentation is caused by time-varryng behaviour"

Jako drugą przyczynę podają fakt, że fragmentacja powstaje kiedy program nagle zmienia sposób użycia pamięci dynamicznej, np. zaczyna zwalniać bloki o małej pojemności i prosić o bloki o dużej. Zachowania te jednak potrafią być schematyczne i dobry alokator powinien to wykorzystywać, albo przynajmniej zadbać o to żeby nie działać powolnie w przypadku najbardziej powszechnych schematów zachowań programu. 

## Zadanie 3
![](https://i.imgur.com/iOuqajA.png)

Autorzy dokumentu *Dynamic Storage Allocation: A Survey and Critical Review* wyróżniają trzy popularne wzorce przydziału pamięci, z których, jak twierdzą co najmniej jeden, a czasami dwa lub nawet wszystkie, zdają się być obecne w większości programów

### Ramps

Jest to przykład zachowania, w którym program akumuluje dane monotonicznie w trakcie swojego działania. Możliwymi powodami takiego zachowania może być potrzeba skonstruowania dużej struktury danych, żeby móc szybko rozwiązać jakiś problem, lub na przykład potrzeba gromadzenia logów.

![](https://i.imgur.com/RlYN7ez.png)

### Peaks

Kolejnym wzorcem może być zachowanie, w którym często alokujemy duże struktury danych, które są zwalniane po wykonaniu danego etapu, i z których pozostaje tylko jego wynik.

![](https://i.imgur.com/ACEBJi2.png)

### Plateus

Ostatni wzorzec występuje w przypadku programów, które na początku swojego istnienia alokują zdecydowaną większość potrzebnej pamięci, a następnie tylko na niej operują.

![](https://i.imgur.com/wOtyA9M.png)

### Jaki jest związek między czasem zycia bloku, a jego rozmiarem?

Rozmiar bloku jest zazwyczaj bezpośrednio powiązany z jego typem oraz przeznaczeniem, dlatego można się spodziewać, że bloki podobnego rozmiaru będą żyły podobnie długo oraz będą zwalniane niedługo po sobie. Dlatego należałoby unikać przeplatania bloków różnego rozmiaru, gdyż możemy spowodować tym więcej wyizolowanych śmierci, a co za tym idzie większą fragmentację zewnętrzną.

### Polityki przydzielania wolnych bloków

* `best fit` -- przeszukujemy całą listę w poszukiwaniu najmniejszego bloku, którego rozmiar jest większy lub równy ilości pamięci, którą chcemy przydzielić. Mimo, że wydaje się, że algorytm mógłby tworzyć dużo bardzo małych bloków, lub nieużytków, gdy znalezione miejsce jest tylko odrobinę za duże, to w praktyce nie sprawia to problemu i algorytm ten zapewnia dobre wykorzystanie pamięci. Problemem jest natomiast jego faktyczna złożoność, gdyż o ile nie posiadamy wolnego bloku dokładnie takiego rozmiaru jak potrzebujemy, to będzie trzeba przejrzeć całą listę wolnych bloków, więc algorytm ten się słabo skaluje kiedy lista wolnych bloków zaczyna rosnąć.
* `first fit` -- przeszukujemy listę wolnych bloków od początku i wybieramy pierwszy o rozmiarze większym lub równym temu zawartemu w prośbie. Mimo, że możemy się po nim spodziewać szybszego działania niż u `best fit`, to w praktyce, przy niektórych politykach zwracania bloków do listy, występuje problem `drzazg`: Ponieważ nasz algortym w pierwszej kolejnści będzie próbował rozdzielac bloki na początku listy, to w efekcie mogą na jej początku skumulować się dużo bardzo małych bloków, które rzadko będą w stanie zadowolić request, a które będą zwiększały czas przeszukiwania listy.
* `next fit` -- próba optymalizacji `first fit`: zamiast przeszukiwać od początku listy, zaczynamy przeszukiwanie w miejscu, w którym ostatnio skończyliśmy (poruszamy się po liście cyklicznej i mamy trawersujacy wskaźnik). Jest to sposób na zaradzenie kumulującym się drzazgom. Problem tej propozycji polega na tym, że ponieważ trawersujący wskaźnik regularnie okrąża listę, to dane pochodzące z różnych etapów programu, o różnym typie i spodziewanym czasem życia, mogą zostać przeplecione, wpływając znacznie na fragmentację zewnętrzną. Może to również zakłócać lokalność programu (sam wskaźnik wykazuje słabą lokalność czasową).

## Zadanie 4

![](https://i.imgur.com/UbNKZdo.png)


Zaczynamy od pustej tablicy z jednym dużym wolnym blokiem:

![](https://i.imgur.com/wWw54Qw.png)

Zbliżenie na początek (dla uproszczenia pomijam wskaźniki wychodzące ze strażnika):

### Best fit

#### alloc(4)

Wykonanie pierwszego alloca będzie polegać na przeszukaniu listy wolnych bloków (w tym przypadku jednoelementowej) i wybraniu tego który pasuje najlepiej (czyli tego jedynego), a następnie rozdzieleniu go:

![](https://i.imgur.com/w65KU8B.png)

#### alloc(8), alloc(4), alloc(4), alloc(10), alloc(6)

Sytuacja przy kolejnych allocach będzie wyglądać identycznie:

![](https://i.imgur.com/NJbgzFN.png)

#### free(C\)

Następnie zwalniamy 3 blok, obok niego nie ma żadnych wolnych bloków więc nie będzie potrzeby złączania. Wolny blok wstawiamy na koniec listy (pustej listy akurat w tym przypadku):

![](https://i.imgur.com/kkdpkIF.png)

#### free(B)

Tym razem trzeba złączyć zwalnianie B z wolnym blokiem leżącym obok:

![](https://i.imgur.com/8nBC4Sv.png)

#### free(F)

Tak samo jak free(C\), tylko musimy nowopowstały blok wrzucić na koniec listy cykliczniej.

![](https://i.imgur.com/jgXiNkK.png)

#### alloc(6)

Postępujemy zgodnie z *best fit* i najlepiej pasuje dziura po F, więc tam wyląduje G.

![](https://i.imgur.com/ulJuesG.png)

#### free(D)

Złączamy puste bloki, tym razem z wcześniejszym:

![](https://i.imgur.com/0MAcMCo.png)

#### alloc(18)

Mamy miejsce żeby go wykonać, ale gdybyśmy rozdzielić blok, to powstałby nowy wolny blok, który byłby zbyt mały żeby pomieścić metadane, więc włączamy go jako nieużytek do przydzielanej pamięci:

![](https://i.imgur.com/QGmUyWk.png)


### First fit

Różnica się pojawi przy ostatnim `alloc(6)`, ponieważ teraz nie bierzemy najlepszego kandydata, tylko pierwszą dziurę na liście która pasuja, a powstały w wyniku splita wolny blok ląduje na końcu listy:

![](https://i.imgur.com/gfQYvc2.png)

Zwolnienie D przebiegnie zwyczajnie:

![](https://i.imgur.com/FURAnzK.png)

Jest jednak problem, gdyż teraz mimo że łącznie wciąż mamy wystarczająco wolnego miejsca na zaspokojenie prośby o 18 bajtów, to nie jesteśmy w stanie przydzielić jej w ciągłym kawałku pamięci, zatem trzeba będzie poszerzyć obszar lub zakończyć prośbę z niepowodzeniem.

## Zadanie 5

### Algorytm kubełkowy

Ideą takiego zarządzania pamięcią jest utrzymywanie tablicy kubełków. Kubełek będzie przechowywał listę wiązaną wolnych bloków pamięci o określonym dla kubełka rozmiarze:

![](https://i.imgur.com/8HD0YoB.png)

oczywiście nie możemy mieć kubełka dla każdego możliwego rozmiaru bloku pamieci, dlatego w praktyce oprócz dedykowanych kubełków dla małych bloków (np. poniżej 512 bajtów) tworzymy kubełki dla rozłącznych klas rozmiarów:

![](https://i.imgur.com/TBhfK8n.png)

### Jak działa malloc?

Otrzymuje prośbę o blok pamięci danego rozmiaru. Następnie wylicza w którym kubełku należy szukać wolnych bloków o takim rozmiarze. W następnej kolejności należy przeszukać kubełek wykorzystując wybraną politykę przydzielania bloków (np. best fit). Jeśli znajdziemy dopasowanie i jest ono idealne, to przydzielamy ten blok użytkownikowi i usuwamy z kubełka.

Jeśli jest za duży i da się go rozdzielić, to należy to zrobić i nowy wolny blok wstawić do odpowiedniego dla jego rozmiaru kubełka.

#### Co jeśli w kubełku nie ma żądanego rozmiaru?

Należy przeszukać kubełki dla większych bloków, w poszukiwaniu jakiegoś który da się rozdzielić, jeśli wszystkie takie kubełki są puste, no to nie ma rady, trzeba przesunąć `brk` i umieścić ten blok na końcu sterty.

### Jak działa free?

Gorliwie scalamy dwa sąsiednie bloki (używając boundary tagów), usuwamy zmerge'owane bloki z kubełków (za pomocą wskaźników które trzymały), następnie powstały blok wstawiamy do kubełka odpowiedniego dla jego rozmiaru.

### Jak to się ma do algorytmów z jedną listą i best fit?

W ogólności algorytmy kubełkowe będą one dobrze aproksymowały `best fit` (będzie to taki `good fit`), bo na start odrzucamy wszystkie bloki których rozmiar pochodzi z innej klasy wielkości. Można nawet rozszerzyć ten algorytm tak, żeby porządkował duże kubełki rosnąco wg. rozmiaru i przeszukiwał je metodą `first fit`, wtedy będzie to dosłownie `best fit` ze znacznie zredukowaną liczbą elementów listy którą trzeba przejrzeć.

### Gdzie należałoby przechowywać węzeł strażnik?

*nie jestem pewien czy zrozumiałem pytanie, ale...*

Jeśli polityka przydzielania nie wymaga inaczej, to możemy zrobić tylko jednego strażnika, do któerego będzie wskazywać każdy koniec listy, a tablica kubełków będzie wskazywać na pierwszy element listy lub na strażnika jeśli lista jest pusta. Jeśli wymagamy cykliczności to chyba sensownie będzie jeśli w tablicy kubełków po prostu będziemy przechowywać strażników.

### Jakie problemy wynikają z leniwego złączania?

Gdybyśmy nie złączali bloków od razu, tylko zwracali je prosto do kubełka i złączali przy przeszukiwaniu, na pewno zwiększylibyśmy gęstość występowania małych kubełków, dzięki czemu po zwolnieniu małego bloku i ponownej próbie przydzielenia tej samej pamięci, na pewno zwrócimy ją praktycznie od razu, bez potrzeby przeszukiwania większych kubełków i ponownego rozdzielania stron. 

Ewentualny problem powstałby gdybyśmy złączali leniwie i tak jak wcześniej przy średniego rozmiaru zapytaniu przeszukiwali od odpowiedniego kubełka wzwyż. Wtedy w mniejszych kubełkach mogłyby znaleźć się sąsiadujące niezłączone bloki zdolne pomieścić poszukiwany rozmiar, ale byłyby one niescalone. (nie możemy po prostu przejść się po wszystkich i scalić, no bo wtedy równie dobrze możemy robić to gorliwie i będzie szybciej)

Dlatego w praktyce istnieją leniwie scalane `fast biny` dla małego (konkretnego) rozmiaru chunków, które są scalane przy zapytaniach o chunki większe od największego rozmiaru obsługiwanego przez fast bin.

![](https://i.imgur.com/jfmhQFe.png)


## Zadanie 6

![](https://i.imgur.com/WsI3GPc.png)


### Alokacja bloku

```c=
static void *alloc_block(int8_t *data, uint8_t len) {
  void *result = NULL;

  while (!END_OF_CHUNK(data)) {
    if (BLOCK_USED(data)) {
      /* TODO: occupied block */
      MOVE_NEXT(data);

    } else if (BLOCK_SIZE(data) == len) {
      /* TODO: free block of exact size */
      BLOCK_HEADER(data) *= -1; // dodatnia długość -- zajęty (patrz BLOCK_USED)
      result = CURRENT_PTR(data);
      break;

    } else if (BLOCK_SIZE(data) > len) {
      /* TODO: free block is too large */
      int8_t diff = BLOCK_SIZE(data) - len;
      BLOCK_HEADER(data) = len;
      NEXT_BLOCK_HEADER(data) = -diff;
      result = CURRENT_PTR(data);
      break;

    } else if (!NEXT_BLOCK_FREE(data)) {
      /* TODO: next block is occupied or does not exists */
      MOVE_NEXT(data);

    } else if (NEXT_BLOCK_SIZE(data) <= len - BLOCK_SIZE(data)) {
      /* TODO: merge two free blocks, but do not allocate */
      /* złączenie bezpieczne, bo suma z przechodniości mniejsza od MAX_BLKS */
      BLOCK_HEADER(data) += NEXT_BLOCK_HEADER(data);

    } else {
      // /* TODO: merge two free blocks and allocate with split */

      // !!!UWAGA!!! Żeby kogoś nie podkusiło, to się może przekręcić i nie 
      // pójdziecie przez to spać:
      // BLOCK_HEADER(data) = BLOCK_HEADER(data) + NEXT_BLOCK_HEADER(data);

      int8_t diff = BLOCK_SIZE(data) + NEXT_BLOCK_SIZE(data) - len;
      assert(diff > 0);
      BLOCK_HEADER(data) = len;
      NEXT_BLOCK_HEADER(data) = -diff;
      result = CURRENT_PTR(data);
      break;

    }
  }
  return result;
}
```

### Zwolnienie bloku

```c=
static void strfree(char *str) {
  if (str == NULL)
    return;
  int8_t *sstr = (int8_t *)str;
#if DEBUG_LEVEL > 0
  assert(sstr[-1] > 0);
  arena_t *ar = find_ptr_arena(&arenas, str);
  assert(ar != NULL);
#if DEBUG_LEVEL > 1
  int8_t *ptr = (int8_t *)ar + sizeof(arena_t);
  while (ptr < sstr - 1) {
    assert(*ptr != 0);
    ptr += (*ptr > 0 ? *ptr : -*ptr);
  }
  assert(ptr == sstr - 1);
#endif
#endif
  /* TODO: mark block as free */
  sstr[-1] *= -1;
}
```

![](https://i.imgur.com/oU5IW3X.png)

### Jak wygląda struktura przechowująca informacje o zajętych i wolnych blokach?

Opis bardzo wygodnie umieszczony jest w treści zadania:

![](https://i.imgur.com/afMzjhp.png)


### Jak przebiegają operacje alloc i free?

* alloc -- przechodzimy po kolejnych arenach w poszukiwaniu takiej, która jest w stanie zmieścić blok, który chcemy zaalokować. Przeszukiwanie wewnątrz areny realizuje procedura `alloc_block`, której kod jest zamieszczony wyżej i która implementuje złączanie, rozdzielanie i przydzielanie wolnych bloków. Jeśli nie znajdzie się taka arena, to alokujemy nową.
* free -- zwyczajnie zmieniamy znak nagłówka bloku


### Pesymistyczna złożoność

`O(n)` dla `stralloc`, gdzie `n` to łączna liczba wszystkich wolnych i zajętych bloków. `O(1)` dla `strfree`.

### Narzut pamięciowy

1 bajt na nagłówek dla każdego bloku.

### Czy fragmentacja wewnętrzna lub zewnętrzna jest istotnym problemem?

Z wewnętrzną dzielnie walczymy dzieląc za duże wolne bloki na mniejsze. Poza tym mamy tylko jeden bajt metadanych wpleciony w blok, więc nie jest to żaden problem.

Zewnętrzna za to stanowi duży problem, gdyż jeśli zwolnimy dużo małych bloków nie leżących obok siebie, to nawet jeśli łącznie będziemy mieli wystarczająco miejsca żeby zadowolić duże zapytanie, to skończymy alokując nową arenę.

### Rozważ scenariusz...

Użytkownik prosi o `n` bajtów i zapisuje `n + 1`, z czego ostatni jest zerem. Użytkownik w ten sposób naruszy naszą strukturę niejawnej listy i zakończy ją przedwcześnie, spowoduje to że wszystkie bloki znajdujące się na tej liście w dalszej kolejności, nigdy nie będą przeszukiwane przy alokacji nowego bloku (zwalnianie będzie dalej działać). Czyli potencjalnie zwiększy się ilość niewykorzystywanego miejsca, ale użytkownik oprócz tego nie powinien być w stanie zauważyć nic niepokojącego. Gorzej jeśli wpisze tam inną wartość...

Jak się przed tym bronić? Ciężko powiedzieć...
https://en.wikipedia.org/wiki/Heap_overflow

## Zadanie 7

![](https://i.imgur.com/n6lZhsN.png)

### Inicjalizacja areny

Poprawka do structa, żeby `sizeof` był dobrze zdefiniowany (`[0]` -> `[]`).

https://stackoverflow.com/questions/9722632/what-happens-if-i-define-a-0-size-array-in-c-c

```c=
#define ARENA_EXTRA                                  
  struct {                                           
    size_t nitems;      /* number of items */        
    size_t nfree;       /* number of free items */   
    void *items;        /* pointer to first item */  
    bitstr_t bitmap[];  /* bitmap of free items */
  }
```

Teraz `sizeof` zwróci rozmiar tak jakby nie było tam elementu `bitmap`.

```c=
static arena_t *init_arena(arena_t *ar) {
  /* TODO: Calculate nitems given ARENA_SIZE, size of arena_t and object_t. */
  size_t header_wo_bitmap = sizeof(arena_t);
  size_t obj_size    = sizeof(object_t);
  size_t free_space  = ARENA_SIZE - header_wo_bitmap;
  size_t nitems      = free_space / obj_size;

  // W tym miejscu nitems jest liczbą elementów tak jakby nie było pola bitmap
  // musimy znaleźć taką wartość nitems żeby wszystko się pomieściło
  while (
    !(header_wo_bitmap 
      + bitstr_size(nitems) 
      + nitems * obj_size 
      <= ARENA_SIZE)) {
    nitems--;
  }

  ar->nitems = nitems;
  ar->nfree = nitems;
  /* Determine items address that is aligned properly. */
  ar->items = arena_end(ar) - nitems * sizeof(object_t);
  return ar;
}
```

### Alokacja bloku

`bit_ffc` znajduje indeks pierwszego zgaszonego bitu. Zwraca `-1` jeśli wszystkie były zapalone.

```c=
static void *alloc_block(arena_t *ar) {
  assert(ar->nfree > 0);
  int index;
  /* TODO: Calculate index of free block and mark it used, update nfree. */
  bit_ffc(ar->bitmap, ar->nitems, &index);
  assert(index != -1);
  bit_set(ar->bitmap, index);
  ar->nfree--;

  return ar->items + sizeof(object_t) * index;
}
```

### Zwolnienie bloku

`bit_tes` - zwraca wartość niezerową jeśli bit jest ustawiony (tutaj pułapka: **niezerowy nie oznacza, że to będzie 1**).

```c=
static void free_block(arena_t *ar, void *ptr) {
  int index = (ptr - ar->items) / sizeof(object_t);
  /* TODO: Determine if ptr is correct and mark it free, update nfree. */
  // sprawdzamy czy to wskaźnik na początek bloku
  assert( (ptr - ar->items) % sizeof(object_t) == 0 ); 
  // sprawdzamy czy index jest sensowny
  assert( index >= 0 && index < ar->nitems );
  // sprawdzamy cza dany blok jest oznaczony jako zajęty
  assert( bit_test(ar->bitmap, index) != 0 );

  bit_clear(ar->bitmap, index);
  ar->nfree++;
}
```

![](https://i.imgur.com/oU5IW3X.png)


### Jak wygląda struktura przechowująca informacje o zajętych i wolnych blokach?

Bitmapa, czyli łańcuch bitów o długości równej liczbie bloków w arenie. Zapalony bit oznacza że blok jest zajęty, a zgaszony, że jest wolny.

### Jak przebiegają operacje alloc i free?

* `free` -- Szukamy areny z danym adresem i ustawiamy odpowiedni bit w bitmapie na 0.
* `alloc` -- Szukamy pierwszej areny z wolnym blokiem, szukamy indeks pierwszego wyzerowanego bitu w bitmapie i ustawiamy go na 1, po czym wyliczamy wskaźnik do odpowiedniego bloku w arenie.

### Pesymistyczna złożoność

`O(m + n)` dla alloca, gdzie `m` to liczba aren `n` to liczba bloków w arenie (bloków jest dużo, więc to przykre). `O(m)` dla free, gdzie `m` to liczba aren (bo `find_ptr_arena` iteruje się po wszstkich tak samo jak alloc).

### Narzut pamięciowy

Jeden bit na blok, plus 32 bajty na arenę (bo taki jest rozmiar `arena_t` bez bitmapy). Niemożliwy do użycia będzie również ewentualny padding na końcu bitmapy wyrównujący ją do wielokrotności rozmiaru bloku.

### Czy fragmentacja wewnętrzna lub zewnętrzna jest istotnym problemem?

Wewnętrzna może być, zależy jak to dokładnie definiujemy. Ponieważ alokujemy zawsze blok tej samej wielkości, więc o ile użytkownik nie potrzebuje dokładnie takiego rozmiaru bloków, to możliwe że jego część nie zostanie przez niego użyta, ale z drugiej strony nie jest tak, że alokujemy więcej niż nas poproszono, plus nie mamy żadnych metadanych wplecionych w alokowany blok (więc albo jest problemem, albo wcale jej nie ma, zależy jak się rozumie tą fragmentację).

Fragmentacja zewnętrzna nie występuje gdyż alokujemy bloki stałego rozmiaru, więc jeśli jakiś się zwolni to zawsze może zostać użyty ponownie dla spełnienia prośby o alokację.
# Systemy Operacyjne -- Lista 7
###### tags: `SO`

## Zadanie 1

### Odwzorowanie pliku w pamięć, a pamięć anonimowa

**Odwzorowanie pliku w pamięć** polega na zmapowaniu pliku na część pamięci wirtualnej procesu, może on w ten sposób czytać\edytować jego zawartość odwołując się do odpowiednich adresów w jego przestrzein adresowej i  pagefaultując strony bezpośrednio z pliku zapisanego na dysku. Pamięć wypełniona jest zatem zawartością pliku.

Natomiast **odwzorowanie pamięci anonimowej** polega na przydzieleniu procesowi pamięci, która nie ma odwzorowania w żadnym pliku. Początkowo każda strona takiej pamięci jest zapełniona zerami (realizowane jest to w ten sposób, że istnieje specjalna strona tylko do odczytu, która zawsze zwraca zero i jądro przydzieli taką stronę do tej pamięci, a dopieru przy próbie zapisu pod jakiś adres powstanie odpowiednia kopia tej strony z modyfikacją (copy-on-write).

### Czym różni się odwzorowanie prywatne od dzielonego

Zmiany dokonywane w pamięci odwzorowanej prywatnie nie są widoczne dla innych procesów, natomiast przy odwzorowaniu dzielonym są, i dodatkowo jeśli pamieć ta jest odwzorowaniem pliku, to zmiany w niej dokonywane są do niego przekazywane.

### Czy pamięć obiektów odwzorowanych prywatnie może być współdzielona?

Tak, i bardzo często tak jest, np. kiedy wykonujemy forka. Trzymanie dwóch kopii takiej samej strony w pamięci byłoby marnotrawstwem, dlatego wykorzystuje się tu mechanizm *copy-on-write* i dopiero przy próbie zmodyfikowania pamięci przez któryś z procesów jakakolwiek strona zostanie powielona, żeby modyfikujący proces mógł wykonać modyfikację, a żeby inne procesy nie zaobserwowały różnicy.

### Dlaczego można odwzorowywać w pamięć tylko blokowe urządzenia?

Ponieważ pozwalają one na swobodny dostęp do danych pod różnym offsetem, urządzenia znakowe zazwyczaj nie posidają danych do których można odwołać się adresem, bo operuje się na nich za pomocą strumieni.

## Zadanie 2

### Scenariusze użycia odwzorowań prywatnych

#### Pliku

Najpowszechniejszym użyciem prywatnego odwzorowania pliku jest ładowanie zawartość segmentu `text` lub `data` z pliku wykonywalnego lub biblioteki współdzielonej.

#### Anonimowe

Używane głównie do dynamicznego alokowania nowej pamięci dla procesu (np. poprzez procedurę `malloc` jeśli poprosimy o odpowiednio dużo pamieci).

### Scenariusze użycia odwzorowań dzielonych

#### Pliku

Używane przy *memory-mapped-I/O*, kiedy mapujemy plik by edytować/czytać go odwołując się do adresów przestrzeni wirtualnej, a nie używając wywołań `write`/`read` (gdyż przy dzielonym odwzorowaniu zmiany są przekazywane do faktycznego pliku). Oprócz tego odwzorowanie to jest przydatne kiedy chcemy skomunikować kilka procesów ze sobą przez plik.

#### Anonimowe

Pozwala na skomunikowanie się ze sobą spokrewnionych procesów, ponieważ dziecko dziedziczy odwzorowanie, a fakt że jest dzielone oznacza, że procesy będą widzieć wprowadzane przez siebie zmiany.

### Jak tworzyć odwzorowania za pomocą mmap?

Sygnatura `mmap` to 

```c=
void *mmap(void *addr, size_t length, int prot, int flags,
                  int fd, off_t offset);
```

gdzie

* `addr` -- sugerowany adres pod którym ma zacząć się odwzorowanie, z tym że jeśli nie będzie on wyrównany, to zostanie wyrównany w górę, a jeśli nie da się pod nim utworzyć odwzorowania, to ten argument zostanie pominięty i to jądro wybierze dogodne miejsce (więc `addr` to tylko sugestia). Dodatkowo jeśli ten argument jest równy `NULL` to jądro samo decyduje gdzie znajdzie się odwzorowanie.
* `length` -- rozmiar odwzorowania w bajtach. Musi być większy od 0.
* `prot` -- w tym argumencie podaje się maskę oznaczającą jakiego rodzaju dostępy będzie można wykonywać do tej pamięci. Możliwe wartości to: `PROT_EXEC`, `PROT_READ`, `PROT_WRITE`, `PROT_NONE`. 
* `flags` -- określa rodzaj mapowania jakie wykonujemy, najbardziej interesujące wartości w tym kontekście to `MAP_SHARED`(dzielone), `MAP_PRIVATE`(prywatne), `MAP_ANONYMOUS`(anonimowe).
* `fd` -- deskryptor pliku który będzie mapowany. Wartość ta jest ignorowana jeśli podamy wcześniej flagę `MAP_ANONYMOUS`.
* `offset` -- mówi ile bajtów od początku pliku wskazanego przez `fd` mamy pominąć w mapowaniu.

### Co się dzieje z odwzorowaniami przy wywołaniu fork?

Są one dziedziczone. Wszystkie strony są oznaczane jako read-only, a w segmentach ustawiana jest flaga copy-on-write, dzięki czemu na początku współdzielona jest cała pamięć, dopiero gdy zaczynają być wykonywane modyfikacje przez jeden z procesów, to strony które mają się różnić miedzy procesami zaczną być duplikowane.

![](https://i.imgur.com/rkexYg8.png)

### Czy exacve tworzy odwzorowania prywatne, czy dzielone?

Muszą to być odwzorowania prywatne, gdyż nie chcemy żeby jakiekolwiek zmiany wykonywane przez program np. na sekcji `data` były przekazywane do pliku wykonywalnego, z którego się ten program uruchamia. Edytowałoby to za każdym razem stan początkowy programu.

### W jaki sposób jądro zwiększa rozmiar stosu?

Stos rośnie poprzez pagefaultowanie. W zależności od implementacji odwołanie się do adresów z pewnego obszaru poniżej stosu powoduje wywołanie błędu strony, który obsługiwany jest poprzez powiększenie rozmiaru stosu, jeśli nie przekroczymy przy tym maksymalnego rozmiaru.

### Kiedy proces otrzyma SIGBUS?

Kiedy utworzymy odwzorowanie pliku w pamięć znacznie większe niż sam plik, tak jak na poniższym obrazku:

![](https://i.imgur.com/ehvKZdX.png)

Wtedy odwołanie się do adresu leżącego wewnętrz strony, której żadna część nie jest zmapowana na plik spowoduje otrzymanie sygnału `SIGBUS`. Po co nam to? Otóż możemy zareagować na to jakoś, np. rozszerzając plik.

## Zadanie 3

Wydruk polecenia `cat /proc/$(grep Xorg)/ status | egrep 'Vm|Rss'`:

```
cezary@cezary-MSI$ cat /proc/$(pgrep Xorg)/status | egrep 'Vm|Rss'
VmPeak: 26257160 kB
VmSize: 26213572 kB
VmLck:         0 kB
VmPin:         0 kB
VmHWM:    177448 kB
VmRSS:    158496 kB
RssAnon:           47400 kB
RssFile:           73804 kB
RssShmem:          37292 kB
VmData:   118948 kB
VmStk:       188 kB
VmExe:      1852 kB
VmLib:    176184 kB
VmPTE:       752 kB
VmSwap:        0 kB
```

### Opis pól

Zbiór roboczy to dane, do których będzie odwoływał się program w określonej chwili, natomiast zbiór rezydentny to dane programu, które obecnie znajdują się fizycznie w pamięci RAM. System będzie się starał przybliżyć zbiór roboczy zbiorem rezydentnym, żeby ograniczyć liczbę pagefaultów.

`VmPeak` -- Najwyższe odnotowany rozmiar zajętej pamięci wirtualnej
`VmSize` -- Obecny rozmiar zajętej pamięci wirtualnej
`VmLck` -- Rozmiar pamięci, która jest zablokowana w pamieci (np przez `mlock`), czyli nie może zostać usunięta z RAMu
`VmPin` -- Podobnie jak `VmLck` nie może zostać usunięta z RAM'u ale dodatkowo znajduje się zawsze w tym samym miejscu, na wypadek gdyby był potrzebny bezpośredni dostęp do pamięci nieopóźniony pagefaultem.
`VmHWM` -- (*High water mark*) największy odnotowany rozmiar zbioru rezydentnego
`VmRSS` -- Rozmiar zbioru rezydentnego
`RssAnon` -- Rozmiar części zbioru rezydentnego odwzorowana anonimowo
`RssFile` -- Rozmiar części zbioru rezydentnego odwzorowana z pliku
`RssShmem` -- Rozmiar dzielonej części zbioru rezydentnego, a konkretnie dzielonych odwzorowań anonimowych, odwzorowań `tmpfs` oraz pamięci dzielonej `system V` (patrz. `shmget`).
`VmData` -- Rozmiar segmentu `data`
`VmStk` -- Rozmiar segmentu stosu
`VmExe` -- Rozmiar segmentu `text`
`VmLib` -- Rozmiar kodu bibliotek współdzielonych
`VmPTE` -- Rozmiar wpisów tablicy stron
`VmSwap` -- Rozmiar prywatnych anonimowych stron wyrzuconych na swap

### Skrypt zliczający VmSize i VmRss

```python=
from glob import glob
from io import TextIOWrapper
from typing import List, Dict

def mem(list: List[str], elem: str) -> int:
  try:
    return list.index(elem)
  except:
    return -1

def getProperties(file: TextIOWrapper, properties: List[str]) -> Dict:
  lines = file.read().split('\n')
  result = {}
  for line in lines:
    line_split = line.split(':')
    if len(line_split) != 2 : continue
    
    [prop, value] = line_split
    index = mem(properties, prop)
    if index != -1 :
      result[prop.strip()] = value.strip()
  return result

total = {
  "VmSize": 0,
  "VmRSS": 0
}

def updateTotal(total : Dict, props : Dict):
  for key, value in total.items():
    try: 
      total[key] = value + int(props[key].split(" ")[0])
    except:
      continue

for path in glob('/proc/[0-9]*'):
  with open(path + '/status', 'r') as file:
    props = getProperties(file, list(total.keys()))
    updateTotal(total, props)

print(total)
```

Wynik skryptu:
```
cezary@cezary-MSI$ python3 zad3.py
{'VmSize': 2898760608, 'VmRSS': 16806160}
```

Dla porównania wydruk `vmstat -s`:
```
cezary@cezary-MSI$ vmstat -s
Pamięć razem:                   16233228 K
Pamięć użyta:                    6444472 K
[...]
```

Dlaczego wyliczona wartość nie pokrywa się z `vmstat`? Najprawdopodobniej dlatego, że zbiory rezydentne procesu wcale nie muszą być rozłączne. Pamieć może być współdzielona, dlatego dużą ilość pamięci dosumowaliśmy kilkukrotnie.

## Zadanie 4

### Slajd z wykładu
![](https://i.imgur.com/uzXJrOR.png)

### Obsługa błędu stronnicowania

Jądro po wystąpnieniu page-faulta zacznie od przejrzenia listy segmentów żeby zlokalizować miejsce w którym wystąpił błąd. Dostarczone do niego zostaną podstawowe informacje potrzebne do obsługi (na podstawie wykładu):

* `fault_addr` -- adres do którego odwołanie wywołało błąd strony
* `fault_pc` -- adres instrukcji która jego wywołała
* `fault_size` -- do ilu bajtów robiony był dostęp
* `fault_prot` -- z jakimi uprawnieniami był robiony dostęp do strony

Obsługa błędu może potoczyć się na kilka sposób:

* Pamięć do której się odwołujemy jest zmapowana z dobrymi bitami `vm_prot` -- wtedy jest to normalny dostęp do pamięci, trzeba ten błąd jakoś naprawić (sprowadzić stronę z dysku/z sieci, skopiować pamięć copy-on-write itp.)
* Na odwołanie się do pamięci nie pozwoliły bity uprawnień -- wtedy musimy zabronić dostępu i np. (tak jak jest na linuksie) wysłać sygnał (może być `SIGSEGV`).
* Odwołujemy się do pamięci, która nie jest w żaden sposób zmapowana, wtedy jest to *segmentation fault* i koniecznie musimy zabronić tego programowi wysyłając sygnał `SIGSEGV`.

### Do czego służą dane struktury jądra?

#### mm_struct::mmap
```c=
struct vm_area_struct *mmap;		/* list of VMAs */
```
Przechowuje listę struktur typu `vm_area_struct`, które definiują odwzorowania w pamięci wirtualnej (ich bity uprawnień, flagi, początek, koniec, itd.).

#### mm_struct::pgd

```c=
pgd_t * pgd;
```

Przetrzymuje wskaźnik na początek (najwyższy katalog) tablicy stron.

### Kiedy wysyłany jest SIGSEGV z kodem SEGV_MAPPER a kiedy z SEGV_ACCERR?

Z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* tabelka 21-2:

![](https://i.imgur.com/4bPzhcr.png)

Czyli kod `SEGV_ACCERR` jest dołączany kiedy wykonaliśmy dostęp do pamięci i nie zgodziły się dla niego bity uprawnień, natomiast kod `SEGV_MAPERR` otrzymamy kiedy odwołanie się do pamięci było do zupełnie niezmapowanej jej części.

### Pomniejsza lub poważna usterka strony

**Poważną usterką strony** nazywamy sytuację, w której naturalnie występujący błąd strony zmusza system do sprowadzenia strony z dysku/sieci do pamięci RAM.

**Pomniejszą usterką strony** nazwiemy sytuację w której poszukiwana strona już jest w fizycznej pamięci, ale np. nie została przyporządkowana do odpytującego procesu (np. korzystają z niej inne procesy), wystarczy ją wtedy zacząć z nimi współdzielić (co nie jest kosztowną modyfikacją).

### Jaką rolę pełni bufor stron?

Z http://gauss.ececs.uc.edu/Courses/c4029/code/memory/understanding.pdf

![](https://i.imgur.com/oXT8LTs.png)

W skrócie: używamy wolną przestrzeń w pamięci operacyjnej, żeby cache'ować w niej strony z dysku, gdyż sprowadzanie ich jest bardzo powolne, a wolna przestrzeń w ramie nie daje żadnej specjalnej przewagi. Jeśli inne procesy będą jej potrzebować, to rozmiar cache'u zostanie obcięty.

Bufory te są stosowane nie tylko przy `mmap`, ale także przy implementacji wywołań `read` i `write`.

## Zadanie 6

### Czu mamy gwarancję, że program mapując plik prywatnie nie zobaczy zmian?

Nie, przez machanizm stronnicowania na żądanie, jest możliwe że część pliku nie będzie załadowana do pamięci, więc jeśli program odwoła się do tej części po wykonanej modyfikacji, to do pamięci operacyjnej zostanie sprowadzona z dysku jej zmodyfikowana wersja.

### Dlaczego nie można edytować plików wykonywalnych kiedy programy z nich są uruchomione

Decydujący czynnik widać w odpowiedzi wyżej, załóżmy że plik wykonywalny jest zmapowany prywatnie w pamięć jakiegoś procesu i teraz wykonamy na nim modyfikację (np. sekcji `text`) -- istnieje prawdopodobieństwo, że strona na której leży modyfikacja nie była wciągnięta do pamięci, bo np. nie jest często używana. W momencie odwołania się do tej strony wciągnięta zostanie zmodyfikowana wersja, zatem jesteśmy w stanie wpłynąć na działanie uruchomionego już programu.

:::info
Tutaj więcej informacji: https://lwn.net/Articles/866493/
:::

### Co złego może się stać?

Na pewno uruchomiony program może ulegnąć dosyć nieoczywistej awarii spowodowanej przemieszaniem jego instrukcji, albo niespójnych danych (skutek takiego *zanieczyszczenia* może być w zasadzie dowolny). Otwiera to też możliwość ataków, ale nie jestem pewien czy często się zdarza że jednocześnie jesteśmy w stanie modyfikować plik i nie możemy po prostu uruchomić ponownie programu.

## Zadanie 7

### Wersja z tworzeniem podprocesów

#### Inicjalizacja tablicy liczb

```c=
/* TODO: Allocate table... */
long *table = mmap(
  NULL, 
  size,
  PROT_READ | PROT_WRITE,
  MAP_SHARED | MAP_ANONYMOUS,
  -1,
  0
);
```

#### Quicksort

```c=
static int QuickSort(long table[], size_t left, size_t right) {
  pid_t pid_left = -1, pid_right = -1, pid = -1;

  /* TODO: If there is more to sort than FORKSORT_MIN start a subprocess. */
  if(right - left > FORKSORT_MIN){
    pid = Fork();
    if(pid > 0) return pid;
  }

  if (left < right) {
    if (right - left <= INSERTSORT_MAX) {
      InsertionSort(table, left, right);
    } else {
      size_t pivot = left + random() % (right - left + 1);
      size_t split = Partition(table, left, right, table[pivot]);

      if (left == split) {
        SwapElem(table, left, pivot);
        split++;
      } else {
        pid_left = QuickSort(table, left, split - 1);
      }

      pid_right = QuickSort(table, split, right);

      /* TODO: Wait for possible children and exit if created a subprocess. */
      if(pid_left != -1)
        Waitpid(pid_left, NULL, 0);
      if(pid_right != -1)
        Waitpid(pid_right, NULL, 0);
      if(pid == 0) exit(0);
    }
  }

  return pid;
}
```

### Wersja bez tworzenia podprocesów

```c=
static void QuickSort(long table[], size_t left, size_t right) {

  /* TODO: If there is more to sort than FORKSORT_MIN start a subprocess. */

  if (left < right) {
    if (right - left <= INSERTSORT_MAX) {
      InsertionSort(table, left, right);
    } else {
      size_t pivot = left + random() % (right - left + 1);
      size_t split = Partition(table, left, right, table[pivot]);

      if (left == split) {
        SwapElem(table, left, pivot);
        split++;
      } else {
        QuickSort(table, left, split - 1);
      }

      QuickSort(table, split, right);
      return;
      /* TODO: Wait for possible children and exit if created a subprocess. */
    }
  }
}
```

### Porównanie czasu wykonania

#### Z podprocesami

```
cezary@cezary-MSI$ /bin/time -p ./forksort                                  
real 2.33
user 12.69
sys 0.72
```

#### Bez podprocesów

```
cezary@cezary-MSI$ /bin/time -p ./forksort
real 8.26
user 8.07
sys 0.18
```

#### Wnioski

Dzięki zrównolegleniu pracy wykonywanej w dwóch rekurencyjnych wywołaniach (dla dużej liczby liczb) udało nam się znacznie zmniejszyć *turnaround time* -- czyli czas potrzebny na zwrócenie wyniku, kosztem *CPU time* czyli faktycznego czasu jaki program spędził wykonując się na procesorach.

Zrównoleglić nie da się np. procedur `InsertionSort` i `Partition`.

##### Prawo Amdahla

![](https://i.imgur.com/ak3f0D5.jpg)

![](https://i.imgur.com/kN8ujvn.png)

![](https://i.imgur.com/5XtmFoJ.png)

![](https://i.imgur.com/lwg9IoF.png)

## Zadanie 8

### doit

```c=
static void doit(const char *path, op_t mode) {
  db_t db;
  db_open(&db, path, 0);

  /* If input file is a terminal device then use standard reading technique. */
  /* TODO: Use fstat instead to handle pipes correctly. */
  struct stat stat;
  Fstat(STDIN_FILENO, &stat);

  bool is_reg = S_ISREG(stat.st_mode);
  bool is_blk_dev = S_ISBLK(stat.st_mode);

  if (!(is_reg || is_blk_dev)) {
    char buf[ENT_LENGTH + 1];
    while (fgets(buf, ENT_LENGTH + 1, stdin))
      consume_line(buf, &db, mode);
  } else {
    char *buf = Mmap(
      NULL, 
      stat.st_size, 
      PROT_READ | PROT_WRITE, 
      MAP_PRIVATE, 
      STDIN_FILENO,
      0 
    );
    char *next = consume_line(buf, &db, mode);
    while(next != NULL) {
      next = consume_line(next, &db, mode);
    }
    Munmap(buf, stat.st_size);
  }

  db_close(&db);
}
```

### db_rehash

```c=
/* Attempt to increase size of database. */
static bool db_rehash(db_t *db, size_t new_size) {
  assert(powerof2(new_size));

  /* Create new database. */
  db_t new[1];

  char *name = alloca(strlen(db->name) + sizeof(".reshash") + 1);
  strcpy(name, db->name);
  strcat(name, ".rehash");
  db_open(new, name, new_size);

  /* Copy everything from old database to new database. */
  /* TODO: Inform OS that we're going to read DB sequentially. */
  size_t old_mem_size = db->size * sizeof(entry_t);
  Madvise(db->entry, old_mem_size, MADV_SEQUENTIAL);

  for (size_t i = 0; i < db->size; i++) {
    if (!db_maybe_insert(new, db->entry[i])) {
      /* Oops... rehashing failed. Need to increase db size and try again. */
      /* TODO: Resize * sizeof(entry_t)move new database, since rehashing failed. */
      db_close(new);
      Unlink(name);
      return false;
    }
  }

  /* TODO Replace old database with new one, remove old database. */
  Munmap(db->entry, old_mem_size);
  Rename(new->name, db->name);

  db->entry = new->entry;
  db->size = new->size;
  free(new->name);
  return true;
}
```

### db_open

```c=
/* Open (`size` = 0) or create (`size` > 0) database from `name` file. */
static void db_open(db_t *db, const char *name, size_t size) {
  assert(powerof2(size));

  int fd = Open(name, O_RDWR | O_CREAT | (size ? O_EXCL : 0), DB_MODE);

  if (size == 0) {
    struct stat sb;
    Fstat(fd, &sb);
    size = sb.st_size / sizeof(entry_t);
    if (size == 0)
      size = 1;
  }

  /* TODO: Setup DB structure, set file size and map the file into memory.
           Inform OS that we're going to read DB in random order. */
  size_t mem_size = size * sizeof(entry_t);
  
  Ftruncate(fd, mem_size);

  db->entry = Mmap(
    NULL, 
    mem_size,
    PROT_WRITE | PROT_READ,
    MAP_SHARED,
    fd,
    0
  );

  db->size = size;
  db->name = strdup(name);

  Madvise(db->entry, mem_size, MADV_RANDOM);

  Close(fd);
}
  
```
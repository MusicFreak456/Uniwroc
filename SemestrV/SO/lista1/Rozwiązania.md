# Systemy Operacyjne -- Lista 1
###### tags: `SO`

| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| X |   | X | X | X | X | X | X |

## Zadanie 1

### Polecenie ps

Narzędzie `ps` służy do zdobywania informacji o obecnie uruchomionych procesach. Użyte flagi oznaczają:

* `-e` -- wyświetl wszystkie procesy
* `-o user,pid,ppid,pgid,tid,pri,stat,wchan,cmd` -- definiuje kolumny w wydruku

Wydruk polecenia `ps -eo user,pid,ppid,pgid,tid,pri,stat,wchan,cmd`:

![](https://i.imgur.com/UP9h93T.png)

Z jego kolumn możemy odczytać poniższe informacje:

* `USER` -- nazwę użytkownika, który jest właścicielem danego procesu, czyli który go uruchomił oraz ma prawa do zarządzania nim.
* `PID` -- identyfikator procesu, czyli unikalna, nieujemna, liczba całkowita jednoznacznie identyfikująca proces.
* `PPID` -- identyfikator rodzica, czyli `PID` procesu, który obecnie jest rodzicem procesu w hierarchii procesów.
* `PGID` -- identyfikator grupy, do której należy proces. Grupa to zbiór jednego lub więcej procesów, przydatna np. przy propagowaniu sygnałów.
* `TID` -- identyfikator wątku, w tym przypadku (bez podania flagi `-L`) `ps` wypisuje wyłącznie procesy, więc ta kolumna będzie równoważna z `PID`.
* `PRI` -- priorytet procesu. (im wyższy tym niższy).
* `STAT` -- stan procesu.
* `WCHAN` -- (?)
* `CMD` -- polecenie wywołujące proces razem z argumentami

### Kto jest rodzicem procesu init?

Zwyczajowo przyjmuje się, że proces init znajduje się w korzeniu hierarchii procesów i nie ma rodzica. Jednak można zauważyć, że podobnie jak proces `kthreadd` o identyfikatorze 2 ma `PPID=0`. Informacje o procesie 0 możemy znaleźć w książce Tanenbauma *Modern Operating Systems*, gdzie czytamy że process zero to pierwszy proces uruchomiony po konfiguracji sprzętu, który następnie inicjlizuje niektóre procesy, m. in. *init*.

### Które z wyświetlonych zadań są wątkami jądra?

Powołując się na APUE -- te w kwadratowych nawiasach. Można też zauważyć, że każdy z nich ma `PPID=2`, jest to spowodowane tym, że w tej wersji linuxa, podobnie jak w przykładzie podanym w 13 rozdziale APUE, istnieje specjalny proces jądra`kthreadd`, którego zadaniem jest tworzenie innych procesów jądra, zatem widnieje jako rodzic każdego z nich.

### Znaczenie liter w kolumnie STAT

![](https://i.imgur.com/vYolv0C.png)

### Które z zadań są wątkami?

Zgodnie z dokumentacją:

![](https://i.imgur.com/tJCcm1P.png)

Czyli te w wąsatych nawiasach.

## Zadanie 3

`proc` to wirtuany system plików, montowany zazwyczaj na ścieżce `/proc` udostępniający bardzo intuicyjny w obsłudze interfejs do odczytywania (i rzadziej zapisywania) informacji zawartych w strukturach jądra systemu, tak jakby były to pliki. 

Np. możemy uzyskać informacje dotyczące wybranego procesu udając się na ścieżkę `proc/[PID]` gdzie znajdziemy takie "pliki" jak:

* `cmdline` -- zwierający polecenie którym wywołany był proces
* `status` -- podstawowe informacje dotyczące procesu, jak np. zużycie pamięci
* `maps` -- informacje o zmapowanych na pamięć plikach oraz stosie czy stercie
* i wiele innych

Oczywiście nie są to faktyczne pliki, a ich zawartość zostanie wygenerowana automatycznie ze stanu struktur jądra w chwili odczytu.

Na ścieżce `/proc` znajdziemy też wiele ogólnych informacji dotyczących wersji systemu, czy obecnego zużycia pamięci operacyjnej itp.

Kolejną ciekawą opcją jaką udostępnia nam ten system plików jest możliwość konfigurowania jądra systemu w czasie wykonania poprzez pisanie do "plików" znajdujących się w "katalogu" `/proc/sys` (np `proc/sys/kernel/hostname`).


### Argumenty programu i zmienne środowiskowe wybranego procesu

Pobieramy id procesu `pidof pulseaudio`.

#### Argumenty programu

Czyli argumenty podane w lini poleceń. Możemy je wyświetlić z pliku `/proc/[PID]/cmdline`

![](https://i.imgur.com/CfzlAaC.png)

#### Zmienne środowiskowe

Zmienne środowiskowe, to ogólnodostępne zmienne, przekazywane niejawnie, które mogą mieć wpływ na zachowanie programu, np. zmienna oznaczająca język użytkownika. Znajdują się w pliku `/proc/[PID]/environ`.

![](https://i.imgur.com/cEIYfYG.png)

#### Plik status

Podejrzyjmy przykładowy plik status procesu:

```
Name:   pulseaudio
Umask:  0077
State:  S (sleeping)
Tgid:   1540
Ngid:   0
Pid:    1540
PPid:   1525
TracerPid:      0
Uid:    1000    1000    1000    1000
Gid:    1000    1000    1000    1000
FDSize: 128
Groups: 4 20 24 27 30 46 114 134 1000 
NStgid: 1540
NSpid:  1540
NSpgid: 1540
NSsid:  1540
VmPeak:  3656560 kB
VmSize:  3394416 kB
VmLck:         0 kB
VmPin:         0 kB
VmHWM:     22480 kB
VmRSS:     22264 kB
RssAnon:            4828 kB
RssFile:           15424 kB
RssShmem:           2012 kB
VmData:    45616 kB
VmStk:       132 kB
VmExe:        64 kB
VmLib:     17868 kB
VmPTE:       256 kB
VmSwap:        0 kB
HugetlbPages:          0 kB
CoreDumping:    0
THP_enabled:    1
Threads:        5
SigQ:   0/63044
SigPnd: 0000000000000000
ShdPnd: 0000000000000000
SigBlk: 0000000000000000
SigIgn: 0000000000001000
SigCgt: 0000000180004a43
CapInh: 0000000000000000
CapPrm: 0000000000000000
CapEff: 0000000000000000
CapBnd: 0000003fffffffff
CapAmb: 0000000000000000
NoNewPrivs:     1
Seccomp:        2
Speculation_Store_Bypass:       thread force mitigated
Cpus_allowed:   fff
Cpus_allowed_list:      0-11
Mems_allowed:   00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000000,00000001
Mems_allowed_list:      0
voluntary_ctxt_switches:        12604275
nonvoluntary_ctxt_switches:     182186
```

Znaczenie pól pliku:

* Uid/Gid -- identyfikator użytkownika/grupy
* Groups -- lista dodatkowych grup
* VmPeak -- największy odnotowany rozmiar pamięci wirtualnej
* VmSize -- rozmiar pamięci wirtualnej
* VmRSS -- rozmiar zbioru rezydentnego
* Threads -- liczba wątków procesu
* (un)voluntary_ctxt_switches -- liczby zmian kontekstu (z dobrej woli[I/O], lub przymusu)

## Zadanie 4

### Wyświetlamy przestrzeń adresową

`sudo pmap $(pidof Xorg)`

### Znaczenie kolumn wydruku

* pierwsza kolumna zawiera adresy wirtualne pod którymi leży blok
* zajmowany rozmiar 
* flagi dostepu
    *  r -- zapis
    *  w -- odczyt
    *  x -- wykonanie
    *  p/s -- prywatny/współdzielony
    *  +/- -- musi zostać skopiowana przy zapisie/została skopiowana
* odwzorowanie pliku na blok pamięci

### Identyfikacja zasobów

**stos** - oznaczony w wydruku [stack], znajduje się na końcu pamięci wirtualnej
**sterta** - pamięć anonimowa umieszona za zmapowanym plikiem wykonywalnym, oznaczona przez [heap] na wydruku z flagą `-X`.
**segmenty** - części pliku binarnego zmapowane na pamięć. Znajdują się na początku przestrzeni adresowej, można je rozróżnić po flagach dostępu.
**pamięć anomimowa** - pamięć nieodwzorowana w systemie plików, na wydruku oznaczona [anon]
**pliki odwzorowane w pamięć** - obszary pamięci posiadające odwzorowanie bit do bita w plikach. Sztandarowym przykładem jest sam plik wykonywalny, ale także biblioteki współdzielone .

## Zadanie 5

Program `lsof` służy do wyświetlania listy otwartych plików, w tym zasobów plikopodobnych, czyli plików specjalnych służących jako np. interfejs komunikacji dla jakiegoś urządzenia. Pojawiają się one w systemie plików tak jak każdy inny plik, jest to abstrakcja zbudowana po to żeby dla uproszczenia dało się przez nie komunikować za pomocą zwykłych wywołań systemowych IO np. `read`, czy `write`.

W programie `lsof` możemy użyć flagi `-p` do wydrukowania listy plików konkretnego procesu.

Rodzaj pliku możemy poznać po kolumnie `TYPE`. Interesujące nas przykłady to:

* pliki zwykłe oznaczone `REG`
* katalogi oznaczone `DIR`
* urządzenia oznaczone `CHR` (character special files), dla plików udostępniających bezpośredni, niebuforowany dostęp do urzadzenia, lub `BLK` dla *block special files*, udostępniających buforowany dostęp i z których zazwyczaj można czytać/pisać w dowolnych rozmiarach i pod dowolnym offsetem
* gniazda sieciowe, oznaczone np. `IPv4`, lub gniazda domeny unixowej oznaczone `unix`
* potoki oznaczone `FIFO`, służące do komunikacji pomiędzy dwoma procesami

## Zadanie 6

### Pomiar czasu wykonania polecenia find /usr

Wynik polecenia `/bin/time -p find /usr`:

```
.
.
.
real 2.37
user 0.80
sys 1.09
```

### Dlaczego real != sys + user?

`sys` to czas spędzony wyłącznie przez ten jeden proces w trybie jądra systemu i analogicznie `user` to czas spędzony w kodzie użytkownika. `real` natomiast to czas jaki upłynął od uruchomienia programu do jego zakończenia. Ponieważ procesy są przeplatane oraz mogą być wykonywane równolegle, czas `real` może być mniejszy lub większy od sumy `sys` i `usr`.

W tym przypadku suma jest mniejsza, więc prawdopodobnie pozostałą część czasu procesor spędził na wykonywaniu innych procesów.

### Dlaczego suma sys i user może być większa od real?

Może stać się tak np. w programie wielowątkowym, ponieważ mogą one być wykonywane równolegle.

Przykładem może być taki prosty wielowątkowy program:

```c=
void *sleep_one_sec(){
    for(int i=0; i < 1000000; i++){
        printf("%d\n", i);
    }
    return NULL;
}

int main() {
    pthread_t thread1_id;
    pthread_t thread2_id;

    pthread_create(&thread1_id, NULL, sleep_one_sec, NULL);
    pthread_create(&thread2_id, NULL, sleep_one_sec, NULL);

    pthread_join(thread1_id, NULL);
    pthread_join(thread2_id, NULL);

    return 0;
}
```

Wydruk pomiaru czasu dla tego programu prezentuje się następująco:
```
.
.
.
real 5.98
user 2.56
sys 6.35
```

### Nałożenie limitu na czas wykonania

Poleceniem `ulimit` z flagą `-t` możemy ograniczyć maksymalny czas wykonania programu liczony w sekundach.

Wydruk po wywołaniu `/bin/time -p find /usr` tym razem:

```
...
/usr/share/libreoffice/help/pl/text/schart/01/05010200.html
/usr/share/libreoffice/help/pl/text/schart/01/04060000.html
Command terminated by signal 9
real 1.16
user 0.40
sys 0.61
```
Jak widać proces został zabity sygnałem nr. 9 (SIGKILL) po dokładnie sekundzie (licząc czas wykonania tylko tego procesu).

## Zadanie 7

Dziecko procesu głównego wraz z narodzinami otrzymuje kopię deskryptorów pliku, które wskazują na ten sam plik, co deskryptor rodzica. Informację o tym możemy znaleźć w manualu funkcji `fork`:

```
*  The  child  inherits  copies of the parent's set of open file descriptors. Each 
file descriptor in the child refers to the same open file description (see open(2))
as the corresponding file descriptor in the parent.  This means that the two file 
descriptors  share  open  file status flags, file offset, and signal-driven 
I/O attributes (see the description of F_SETOWN and F_SETSIG in fcntl(2)).
```

### Czy zamknięcie pliku w dziecku zamyka plik w rodzicu?

```c=
static void do_close_2(int fd) {
  /* TODO: In the child close file descriptor, in the parent wait for child to
   * die and check if the file descriptor is still accessible. */

  int pid = Fork();

  if(pid == 0){ //child
    Close(fd);
    printf("Child (pid: %d) - File closed\n", getpid());
  }
  else { //parent
    int child_status;
    waitpid(pid, &child_status, 0);
    Read(fd, buf, LINE1);
    printf("Parent (pid: %d) - line read: %s", getpid(), buf);
    Close(fd);
  }

  exit(0);
}
```

#### Wydruk programu

```
Child (pid: 9415) - File closed
Parent (pid: 9414) - line read: Write programs that do one thing and do it well.
```

### Czy zamknięcie pliku w rodzicu zamyka plik w dziecku?

```c=
static void do_close_1(int fd){

  int pid = Fork();

  if(pid == 0){ //child
    sleep(5);
    Read(fd, buf, LINE1);
    printf("Child (pid: %d) - line read: %s", getpid(), buf);
    Close(fd);
  }
  else {
    Close(fd);
    printf("Child (pid: %d) - File closed\n", getpid());
  }

  exit(0);
}
```

#### Wydruk programu

```
Parent (pid: 9902) - File closed
Child (pid: 9903) - line read: Write programs that do one thing and do it well.
```

### Co się stało?

Same deskryptory są **kopiowane** do dziecka, to znaczy, że posiada on ich kopię na własność i zwolnienie z tej listy deskryptora `fd` nie wpływa na listę deskryptorów rodzica i na odwrót.

#### Kiedy zatem plik jest faktycznie zwalniany?

W dokumentacji `close` czytamy:

```
If fd is the last file descriptor referring to the underlying open file description 
(see open(2)), the resources  associated  with  the  open file  description  
are  freed;  if  the  file descriptor was the last reference to a file which 
has been removed using unlink(2), the file is deleted.
```

### Czy odczyt w jednym procesie zmienia pozycję kursora w drugim?

```c=
static void do_read(int fd) {
  /* TODO: Spawn a child. Read from the file descriptor in both parent and
   * child. Check how file cursor value has changed in both processes. */

  printf("Parent (pid: %d) - position before read: %ld\n", getpid(), lseek(fd, 0, SEEK_CUR));

  int pid = Fork();

  if(pid == 0){ //child
    Read(fd, buf, LINE1);
    printf("Child (pid: %d) - position after read: %ld\n", getpid(), lseek(fd, 0, SEEK_CUR));
  } 
  else { //parent
    int child_status;
    waitpid(pid, &child_status, 0);
    Read(fd, buf, LINE2);
    printf("Parent (pid: %d) - position after read: %ld\n", getpid(), lseek(fd, 0, SEEK_CUR));
  }

  Close(fd);
  exit(0);
}
```

#### Wydruk programu

```
Parent (pid: 10325) - position before read: 0
Child (pid: 10326) - position after read: 49
Parent (pid: 10325) - position after read: 82
```

Jak widać pozycja kursora została przesunięta w procesie, który nie wykonywał instrukcji `Read`.

## Zadanie 8

### ndselect

```c=
static int ndselect(int n) {
  /* TODO: A loop is missing here that spawns processes and waits for them! */
  
  for (int i = 0; i < n; i++)
  {
    int pid = Fork();
    if(pid == 0) return i;
    else {
      int child_status;
      waitpid(pid, &child_status, 0);
    }
  }

  exit(0);
}
```

### main

```c=
int main(int argc, char **argv) {
  if (argc != 2)
    app_error("Usage: %s [SIZE]", argv[0]);

  int size = atoi(argv[1]);

  if (size < 3 || size > 9)
    app_error("Give board size in range from 4 to 9!");

  int board[size];
  memset(board,-1,size*sizeof(int));
  
  /* TODO: A loop is missing here that initializes recursive algorithm. */
  for(int i = 0; i < size; i++){
    int proposed = ndselect(size);
    board[i] = proposed;
    for(int j = 0; j < i; j++){
      if(conflict(j,board[j], i, proposed)) exit(0);
    }
  }

  print_board(size, board);
  return 0;
}

````
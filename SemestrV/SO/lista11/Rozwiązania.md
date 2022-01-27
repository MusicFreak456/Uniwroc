# Systemy Operacyjne -- Lista 11
###### tags: `SO`


## Zadanie 1

![](https://i.imgur.com/m2oDS89.png)

### Punkt montażowy 

W systemach UNIX wszystkie pliki znajdują się w pojedynczym drzewie katalogów, którego korzeń to `/`. Pliki mogą pochodzić z różnych systemów plików, do których dostęp odbywa się przez specjalne katalogi, pod które są podczepione. Takie katalogi nazywamy punktami montażowymi.

![](https://i.imgur.com/nRX0XKp.png)
[grafika pochodzi z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* (LPI)]

### Lista zamontowanych systemów plików

Możemy wyświetlić ją poleceniem `findmnt -l`:

```
TARGET                    SOURCE       FSTYPE      OPTIONS
/                         /dev/sdb     ext4        rw,relatime,discard,errors=remount-ro,data=ordered
/mnt/wsl                  tmpfs        tmpfs       rw,relatime
/init                     tools[/init] 9p          ro,relatime,dirsync,aname=tools;fmask=022,loose,access=client,trans=fd,rfd=6,wfd=6
/dev                      none         devtmpfs    rw,nosuid,relatime,size=6478688k,nr_inodes=1619672,mode=755
/sys                      sysfs        sysfs       rw,nosuid,nodev,noexec,noatime
/proc                     proc         proc        rw,nosuid,nodev,noexec,noatime
/dev/pts                  devpts       devpts      rw,nosuid,noexec,noatime,gid=5,mode=620,ptmxmode=000
/run                      none         tmpfs       rw,nosuid,noexec,noatime,mode=755
/run/lock                 none         tmpfs       rw,nosuid,nodev,noexec,noatime
/run/shm                  none         tmpfs       rw,nosuid,nodev,noatime         rw,nosuid,nodev
[...]
```

Przykładami instancji *pseudo systemów plików* na tej liście są np. `proc`, który udostępnia informacje o procesach, które są zawarte w jądrze, czy `sysfs`, który udostępnia informacje o stanie struktur jądra niezwiązanych z procesami.

#### Znaczenie kolumn

* `TARGET` -- punkt montażowy
* `SOURCE` -- zamontowane urządzenie
* `FSTYPE` -- system plików
* `OPTIONS` -- atrybuty danej instancji

#### Znaczenie atrybutów

* `noatime` -- czasy ostatniego dostępu do plików nie są aktualizowane. Pozwala uzyskać znaczny wzrost wydajności w scenariuszu, w którym bardzo często odczytujemy pliki, gdyż odczyt nie implikuje konieczności zapisania niczego do systemu plików.
* `noexec` -- oznacza, że żaden plik wykonywalny z tego systemu nie może zostać uruchomiony. Przydaje się to jeśli nie chcemy przypadkiem uruchomić jakiegoś szkodliwego programu/skryptu, który podszywa się pod plik z danymi.
* `sync` -- atrybut ten sprawia, że każdy zapis do plików i katalogów w tym systemie plików, będzie wykonywany od razu, bez buforowania. Przydatne jeśli istnieje ryzyko nagłego zerwania połączenia z urządzeniem, na którym ten system się znajduje.

## Zadanie 2

![](https://i.imgur.com/souhJnT.png)

**blok** -- niewielki zbiór sektorów pamięci.
**grupa bloków** -- zbiór sekwencyjnie ułożonych bloków. System będzie starał się przydzielać do pliku bloki z jednej grupy, żeby jak najwięcej z nich dało się czytać sekwencyjnie (np. bez skakania głowicą po dysku).
**superblok** -- specjalny blok zawierający istotne metadane systemu plików, dla bezpieczeństwa przechowuje się wiele jego kopii.
**tablica deskryptorów grup bloków** -- tablica zawierająca położenie bitmap zajętości każdej grupy bloków. Jej również przechowywać będziemy kilka kopii.


### Wzory

#### Rozmiar bloku

Zgodnie ze wzorem z literatury:

```
block size = 1024 << s_log_block_size
```

#### Liczba i-węzłów w grupie bloków

```
s_inodes_per_group
```

#### Liczba bloków w grupie bloków

```
s_blocks_per_group
```

#### Liczba wpisów w tablicy deskryptorów grup bloków

```
ceil(s_blocks_count / s_blocks_per_group)
```

### Składowe grupy bloków

![](https://i.imgur.com/3uIDt2T.png)
[grafika pochodzi z wykładu]

Superblok i tablica deskryptorów grup znajdują się tylko w wyszczególnionych blokach. Po nich (albo na początku) znajdują się bitmapy zajętości. Później tablica i-węzłów, a reszta miejsca przeznaczona jest na bloki z danymi.

#### Rozmiary

* Superblok -- 1 blok
* Tablica deskryptorów grup -- `ceil((ceil(s_blocks_count / s_blocks_per_group) * sizeof(group_desc)) / blksz)`
* Bitmapa zajętości bloków -- 1 blok
* Bitmapa zajętości i-węzłów -- 1 blok
* Tablica i-węzłów -- `ceil(s_inodes_per_group * s_inode_size / blksz)`
* Bloki na dane użytkownika -- reszta bloków

### Które grupy przechowują kopię superbloku i tablicy deskryptorów?

Zgodnie z załączoną literaturą są to grupy nr. 0, 1 i o numerach będących potęgami 3,5 i 7.

## Zadanie 3

![](https://i.imgur.com/nyWiZp7.png)

**wdrażanie synchroniczne** -- oznacza, że mamy pewność, że po wykonaniu operacji jej efekty zostały faktycznie przekazane do znajdującego się pod systemem hardware'u.

**spójność systemu plików** -- określa stopień w jakim struktury systemu opisują jego stan faktyczny.

### Dopisywanie n bloków do pliku

Lista kroków:

* Zlokalizuj i-węzeł pliku.
<!-- * Powtórz n razy:
  * Wybierz wolny blok z grupy, w której znajduje się i-węzeł. Jeśli nie ma już wolnych bloków w tej grupie, wybierz z innej.
  * Zapisz jego zawartość.
  * Zmniejsz wartość licznika `s_free_blocks_count` w superbloku.
  * Zaktualizuj licznik wolnych bloków grupy `bg_free_blocks_count`.
  * Zaktualizuj bitmapę wolnych bloków `bg_block_bitmap`
  * Dodaj wskaźnik na blok do i-węzła:
    * Oblicz indeks na podstawie `i_blocks` (liczby bloków 512 bajtowych). 
    * Jeśli to pierwszy wpis w bloku, należy go zaalokować. -->

* Wybierz jak najwięcej wolnych bloków z grupy, w której znajduje się i-węzeł, resztę dobierz z innych.
* Zapisz zawartość bloków.
* Zmniejsz wartość licznika `s_free_blocks_count` w superbloku.
* Dla każdej grupy bloków z której wybrano wolne bloki:
  * Zaktualizuj licznik wolnych bloków `bg_free_blocks_count`.
  * Zaktualizuj bitmapę wolnych bloków `bg_block_bitmap`.
* Dla każdego wybranego bloku:
  * Oblicz jego indeks na podstawie `i_blocks` (liczby bloków 512 bajtowych).
  * Jeśli to pierwszy wpis w niebezpośrednim bloku, zaalokuj ten blok i ewentualnie bloki na ścieżce do niego.
  * Zapisz wskaźnik na blok (i ewentualnie uzupełnij wskaźniki w blokach pośrednich).
  * Zaktualizuj licznik `i_blocks`.

## Zadanie 4

![](https://i.imgur.com/xNun3vX.png)


### Dlaczego rename kończy się EXDEV?

Polecenie `rename` operuje wyłącznie na wpisach w katalogu, czyli parach `(nazwa, inode)`. Ponieważ numer i-węzła jest ważny tylko w obrębie jednego systemu plików, nie można dodać twardego dowiązania do niego w innym.

### Implementacja przenoszenia między katalogami

#### Reprezentacja katalogów -- przypomnienie

![](https://i.imgur.com/PAlXdDB.png)

O katalogu można myśleć jak o liście wiązanej wpisów typu `dirent` które zawierają pole opisujące do którego i-węzła wpis się odnosi, rozmiar wpisu w bajtach, typ pliku (ta informacja jest powielana z inode'a dla wydajności), długość nazwy oraz samą nazwę. 

O wielkości wpisu można myśleć jak o względnym wskaźniku na kolejny wpis. Wpisy mogą mieć padding większy niż ten wynikający z długości nazwy, takie dodatkowe miejsce nazywa się nieużytkiem. Nie dopuszcza się także żeby pomiędzy wpisami znajdowała się pusta przestrzeń nienależąca do żadnego wpisu.

#### Rename -- semantyka

Wywołanie `rename(2)` ma dosyć złożoną semantykę. Przyjmuje dwa argumenty:

* `oldpath` -- istniejąca ścieżka do pliku
* `newpath` -- ścieżka na której ma się znaleźć plik

W najprostszym przypadadku po prostu tworzymy wpis dla `newpath` i uswamy `oldpath`. Przypadki brzegowe na które musimy uważać to:

* jeśli `newpath` już istnieje, to go nadpisujemy.
* jeśli `newpath` i `oldpath` odnoszą się do tego samego pliku nie robimy nic. (W szczególności niczego nie usuwamy).
* nie rozwiązujemy dowiązań symbolicznych, traktujemy je jak zwykłe pliki.

`rename` potrafi też przenosić katalogi, ale dla uproszczenia to pominiemy.

#### Lista kroków

Przenosimy plik `P`, o nazwie (w katalogu wskazywanym przez `oldpath`) `p` i numerze i-węzła `x` do pliku o nazwie `q` w ścieżce `newpath`.

1. Lokalizujemy wpis katalogu dla `oldpath` i wczytujemy i-węzeł, do którego wskazuje.
2. Lokalizujemy katalog docelowy. Trawersujemy jego zawartość:
   * Pomijamy wpisy o numerze i-węzła 0. 
   * Jeśli znajduje się w nim plik o nazwie `q` i wskazuje na numer i-węzła `x`, kończymy działanie.
   * Jeśli istnieje w nim plik nazwie `p`, ale wskazuje na inny i-węzeł `y` to:
     * Zwiększamy liczbę odwołań do i-węzła `x`.
     * Zamieniamy `y` na `x` we wpisie.
     * Zmniejszamy liczbę odwołań do i-węzła `y`. 
     * Przechodzimy do punktu [6].
3. Ponownie trawersujemy zawartość katalogu docelowego, próbując wybrać miejsce na wpis:
    * Jeśli trafimy na numer i-węzła 0 i jego wielkość pozwala na zapisanie nazwy, to dodajemy wpis w jego miejscu.
    * rozmiar paddingu (części nazywanej nieużytkiem) obecnego wpisu pozwala na umieszczenie w nim dodawanego wpisu i nie przetnie on granicy między blokami – dodajemy wpis za nazwą pliku w obecnie przeglądanym wpisie.
    * wpis ma długość 0 -- sygnalizujemy błąd.
    * wpp. trawersujemy dalej (nie dojdziemy do końca, bo zakładamy że jest miejsce).
4. Zwiększamy liczbę odwołań w i-węźle `x`. 
5. Dodajemy wpis w wybranym miejscu.
6. Usuwamy oryginalny wpis:
    * Lokalizujemy oryginalny wpis w katalogu z `oldpath`.
    * Usuwamy go:
      * Jeśli to pierwszy wpis, ustawiamy numer i-węzła na 0.
      * wpp. zwiększamy rozmiar poprzedniego wpisu. Wpis, który chcemy usunąć stanie się nieużytkiem poprzedniego wpisu.
    * Zmniejszamy liczbę odwołań do `x`.

## Zadanie 5

![](https://i.imgur.com/UlhWMeH.png)

### Lista kroków

Usuwamy plik o nazwie `p`.

* Trawersujemy wpisy w katalogu do momentu odnalezienia wpisu o nazwie `p`.
* Pobieramy numer i-węzła i go wczytujemy.
* Usuwamy wpis w katalogu:
	* Jeśli to pierwszy wpis, ustawiamy numer i-węzła na 0.
    * wpp. zwiększamy rozmiar poprzedniego wpisu. Wpis, który chcemy usunąć stanie się nieużytkiem poprzedniego wpisu.
* Zmniejszamy licznik dowiązań w i-węźle.
* Jeśli licznik spadł do 0 i nie ma otwartych deskryptorów:
	* Oznaczamy i-węzeł jako zwolniony
	* Oznaczamy bloki i-węzła jako zwolnione

### Kiedy plik jest faktycznie usuwany?

W metadanych i-węzła utrzymywane są dwa liczniki: 
* jeden znajduje się w i-węźle i zlicza liczbę twardych dowiązań do niego.
* drugi jest po stronie jądra i zlicza liczbę otwartych deskryptorów plików odwołujących się do danego i-węzła.

Plik jest faktycznie usuwany tylko jeśli oba te liczniki spadną do 0, czyli jeśli usuniemy wszystkie twarde dowiązania do niego, oraz programy pozamykają otwarte deskryptory do niego się odnoszące.

### Kiedy możliwe jest odkasowanie?

Warunkiem koniecznym jest żeby bloki, które należały do pliku nie zostały przydzielone innym plikom i nadpisane.

Sytuacjami, w których w oczywisty sposób da się odwrócić usunięcie pliku, to:

* jeśli istnieją inne odwołania do niego w systemie plików. Wtedy wystarczy odtworzyć twarde dowiązanie.
* jeśli plik jest wciąż otwarty przez inne programy, wtedy jego zasoby nie zostały zwolnione i i-węzeł wciąż istnieje, czyli możemy się do niego dowiązać.

Istnieją również narzędzia potrafiące odzyskać nawet zwolnione pliki jeśli ich bloki nie zostały nadpisane.

## Zadanie 6

![](https://i.imgur.com/4JJjQOG.png)


### Dowiązanie twarde

Przy jego tworzeniu dodaje się nowy wpis do katalogu zawierający nazwę pod jaką dany plik ma w nim figurować oraz numer i-węzła. W i-węźle trzeba też zaktualizować liczbę referencji, służy ona do rozpoznania momentu, w którym nic już się do niego nie odnosi i miejsce, które zajmuje odpowiadający mu plik, można zwolnić.

Można je tworzyć poleceniem `ln`. W systemie `ext2` może istnieć wiele odwołań do tego samego pliku pod różnymi nazwami.

### Dowiązanie symboliczne

Przy jego tworzeniu po prostu powstaje nowy plik (w jego i-węźle oznacza się że jest *symlinkiem*), a jego zawartością jest ścieżka (bezwzględna lub względna) do pliku na który ma wskazywać. 

Przewagą takich dowiązań jest fakt, że można tworzyć je pomiędzy systemami plików, ale za to nie mamy pewności że plik, do którego się odnosi dalej istnieje.

Tworzymy je poleceniem `ln` z flagą `-s`.

#### Gdzie przechowywana jest zawartość symlinka?

Na podstawie *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* (LPI).

Jeśli wielkość napisu jest wystarczająco mała, to zostanie on umieszczony wewnątrz i-węzłą, w miejscu gdzie normalnie przechowywane by były wskaźniki na bloki z danymi.

![](https://i.imgur.com/AR4JySw.png)
[grafika pochodzi z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* (LPI)]


### Jak stworzyć pętlę?

Wystarczy utworzyć dowiązanie do katalogu, który znajduje się na ścieżce między korzeniem drzewa systemu plików, a tym dowiązaniem.

Np. tutaj tworzymy dowiązanie do katalogu w którym jesteśmy.

```
cezary@cezary-MSI$ ln -s ../foo foo/testdir
cezary@cezary-MSI$ ls -al foo
razem 8
drwxrwxr-x  2 cezary cezary 4096 sty 11 21:21 .
drwxr-xr-x 40 cezary cezary 4096 sty 11 21:21 ..
lrwxrwxrwx  1 cezary cezary    6 sty 11 21:21 testdir -> ../foo
```

Niestety narzędzia na linuksie rzadko chcą podróżować dwa razy przez ten sam katalog, więc ciężko zaprezentować to zapętlenie.

```
cezary@cezary-MSI$ ls -LR foo
foo:
testdir
ls: foo/testdir: nie będzie wylistowany katalog już wylistowany poprzednio
```

#### Kiedy jądro zwróci ELOOP?

Zgodnie z podręcznikiem `path_resolution(7)`:

```
In order to protect the kernel against stack
overflow, and also to protect against denial of service, there
are limits on the maximum recursion depth, and on the maximum
number of symbolic links followed.  An ELOOP error is returned
when the maximum is exceeded ("Too many levels of symbolic
links").
```

#### Dlaczego nie da się tego zrobić z dowiązaniami twardymi?

Gdyż właśnie z tego powodu zabroniono nam robić dowiązania twarde do katalogów.

## Zadanie 7

![](https://i.imgur.com/VzPKr3A.png)

### Czemu fragmentacja systemu plików jest zła?

Fragmentacja systemu plików to zajwisko, przy którym pliki przestają być umieszczane w ciągłych obszarach pamięci, lub mamy dużo małych wolnych i ciągłych obszarów pamięci, zamiast kilku dużych.

Jest to złe zjawisko, gdyż szybciej się odczytuje dane położone sekwencyjnie, zarówno w tradycyjnych dyskach, gdzie inaczej tracimy czas na przesuwanie głowicy, jak i w napędach półprzewodnikowych.

### W jaki sposób odroczony przydział bloków zapobiega fragmentacji?

Przewaga odraczania przydziału bloków polega na tym, że zbierając prośby o alokację, możemy "widzieć przyszłość" i sklejać wiele próśb dotyczących tego samego pliku w jedną, co pozwala szukać lepiej dopasowanych wolnych obszarów pamięci, co zmniejsza fragmentację.

Dzięki odraczaniu można też uniknąć niepotrzebnych alokacji bardzo krótko żyjących plików.

### Wytłumacz jak zakresy pozwalają zmniejszyć metadane?

Wcześniejsze systemy `ext*` przechowywały wskaźnik każdego pojedynczego bloku zaalokowanego w ramach pliku. 

![](https://i.imgur.com/miIXazO.png)
[grafika pochodzi z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* (LPI)]

Mimo, że mogło to być wystarczająco wydajne dla małych plików, to w praktyce dla dużych plików generowało ogromny narzut pamięciowy. Dlatego w `ext4` wykorzystano obserwację, że duże pliki mimo że składają się z ogromnej liczby bloków, to wiele z nich leży w ciągłych obszarach pamieci.

Zaproponowano więc inną drzewiastą strukturę, w której liścmi nie są pojedyncze bloki, a zakresy (ang. *extents*) bloków.

![](https://i.imgur.com/BHQAtuO.png)

Dzięki temu, nawet duże pliki mogą być reprezentowane przez małą ilość metadanych, jeśli tylko nie są mocno pofragmentowane.

4 zakresy są zapisane bezpośrednio w strukturze i-węzłą, dzięki czemu reprezentacja ta jest wciąż wydajna dla małych plików.

### Czy liczba wolnych bloków po defragmentacji może wzrosnąć?

Tak, ponieważ nie przechowujemy teraz wskaźnika na każdy możliwy blok, istnieje możliwość, że po defragmentacji plik będzie wymagał znacznie mniej (jeśli nie po prostu jednego) zakresów bloków i bedzie się je dało przechować wewnątrz i-węzła, lub po prostu w mniejszej liczbie bloków.

### Jak mógłby wyglądać najprostszy algorytm defragmentacji?

* Dla każdego pliku `P`:
  * Stwórz tymczasowy i-węzeł
  * Zaalokuj do niego na raz liczbę bloków z `P`. Oczekujemy otrzymać w miarę ciągły przydział.
  * Skopiuj dane z pliku `P` do pliku tymczasowego.
  * Przepnij zakresy i-węzła tymczasowego do i-węzła pliku `P`.
  * Zwolnij tymczasowy i-węzeł i stare bloki.

## Zadanie 8

![](https://i.imgur.com/eQ474XW.png)

`debugfs` to narzędzie do debugowania systemów plików `ext2, ext3 i ext4`. Po uruchomieniu pracuje w trybie interaktywnym.

Najpierw musimy wybrać urządzenie do otwarcia znajdującego się na nim systemu plików.

```
open /dev/nvme0n1p6
```

### Fragmentacja systemu plików

Po otwarciu systemu plików wystarczy wpisać polecenie `freefrag`. Otrzymamy statystyki dotyczące tego jakiej wielkości wolne ciągłe obszary pamięci przeważają w naszym systemie systemie.

```
Min. free extent: 4 KB 
Max. free extent: 37476 KB
Avg. free extent: 112 KB
Num. free extent: 5358

HISTOGRAM OF FREE EXTENT SIZES:
Extent Size Range :  Free extents   Free Blocks  Percent
    4K...    8K-  :          1487          1487    0.98%
    8K...   16K-  :          1177          2925    1.92%
   16K...   32K-  :           957          5103    3.35%
   32K...   64K-  :           807          8683    5.70%
   64K...  128K-  :           660         15146    9.94%
  128K...  256K-  :           102          4238    2.78%
  256K...  512K-  :            49          4893    3.21%
  512K... 1024K-  :            27          5075    3.33%
    1M...    2M-  :            40         15024    9.86%
    2M...    4M-  :            20         13992    9.18%
    4M...    8M-  :            20         28476   18.68%
    8M...   16M-  :             9         23923   15.69%
   16M...   32M-  :             2         14091    9.24%
   32M...   64M-  :             1          9369    6.15%
debugfs:  
```

### Informacje o grupach bloku

Po wpisaniu polecenia `stats`, informacje o grupach bloków znajdują się na końcu.

```
 Group  0: block bitmap at 1027, inode bitmap at 1043, inode table at 1059
           4151 free blocks, 222 free inodes, 542 used directories, 0 unused 
           inodes
           [Checksum 0xc601]
 Group  1: block bitmap at 1028, inode bitmap at 1044, inode table at 1568
           0 free blocks, 10 free inodes, 841 used directories, 0 unused inodes
           [Checksum 0xc27d]
 Group  2: block bitmap at 1029, inode bitmap at 1045, inode table at 2077
           0 free blocks, 6283 free inodes, 719 used directories, 6243 unused 
           inodes
           [Checksum 0xb902]
 Group  3: block bitmap at 1030, inode bitmap at 1046, inode table at 2586
           0 free blocks, 8144 free inodes, 0 used directories, 8144 unused 
           inodes
           [Inode not init, Checksum 0xdf1f]
[...]
```

### Zakresy bloków pliku

Na wybranym pliku używamy polecenia `extents`.

```
extents /home/cezary/.xsession-errors.old
```

```
Level Entries       Logical          Physical Length Flags
 0/ 1   1/  1     0 -    46 2670536               47
 1/ 1   1/  8     0 -     4 2394660 - 2394664      5 
 1/ 1   2/  8     5 -     6 2416155 - 2416156      2 
 1/ 1   3/  8     7 -     7  576012 -  576012      1 
 1/ 1   4/  8     8 -     9 2784278 - 2784279      2 
 1/ 1   5/  8    10 -    10 2416169 - 2416169      1 
 1/ 1   6/  8    11 -    14  575581 -  575584      4 
 1/ 1   7/  8    15 -    44 1484160 - 1484189     30 
 1/ 1   8/  8    45 -    46 2728000 - 2728001      2 
 ```
 
To co tutaj widzimy zostało dodane w systemie plików `ext4` żeby ograniczyć  ilość metadanych potrzebną do przechowania mapowania plików na bloki. Zamiast przechowywać wskaźnik na każdy pojedynczy blok, przechowujemy ciągłe obszary nazywane `extent` i zapamiętujemy tylko ich początek i koniec.

### Zawartość i-węzła dowiązania sybolicznego

Polecenie `idump` wypisuje zawartość i-węzła w postaci szesnastkowej oraz ASCII. Zastosujemy je do jakiegoś dowiązania symbolicznego

Dowiązanie:
```
[...]
drwxr-xr-x 5 cezary cezary 4096 paź  8 19:04 130
[...]
lrwxrwxrwx 1 cezary cezary    3 paź  8 19:04 current -> 130
```

Polecenie:
```
debugfs:  idump /home/cezary/snap/discord/current
```

Wydruk:
```
0000  ffa1 e803 0300 0000 4e4d dd61 007a 6061  ........NM.a.z`a
0020  007a 6061 0000 0000 e803 0100 0000 0000  .z`a............
0040  0000 0000 0100 0000 3133 3000 0400 0000  ........130.....
0060  0000 0000 0000 0000 0000 0000 0000 0000  ................
*
0140  0000 0000 6799 479d 0000 0000 0000 0000  ....g.G.........
0160  0000 0000 0000 0000 0000 0000 9edf 0000  ................
0200  2000 0b21 981f 6995 981f 6995 6cb3 c7e2   ..!..i...i.l...
0220  007a 6061 981f 6995 0000 0000 0000 0000  .z`a..i.........
0240  0000 0000 0000 0000 0000 0000 0000 0000  ................
*
```
Widać że jego zawartość (czyli `130`) jest zawarta w i-węźle.

### Do jakiego pliku należy dany blok

Najpierw znajdziemy jakiś blok, o którym wiemy do którego pliku należy za pomocą polecenia `blocks`.

```
debugfs:  blocks /home/cezary/.xsession-errors
2669920 619029 619030 619031 619032 619033 2513831 791581 [...]
```

Żeby odwrócić tą operację najpierw możemy dowiedzieć się do jakiego i-węzła należy wybrany blok za pomocą polecenia `icheck`.

```
debugfs:  icheck 2669920
Block   Inode number
2669920 665380
```

A następnie dowiedzieć się jakie pliki odnoszą się do tego i-węzła za pomocą polecenia `ncheck`.

```
debugfs:  ncheck 665380
Inode   Pathname
665380  /home/cezary/.xsession-errors
```

### Reprezentacja liniowa katalogu

Katalog: `/home/cezary/snap/discord`

```
cezary@cezary-MSI$ ls -al
razem 16
drwxr-xr-x 4 cezary cezary 4096 paź  8 19:04 .
drwxr-xr-x 3 cezary cezary 4096 paź  8 19:23 ..
drwxr-xr-x 5 cezary cezary 4096 paź  8 19:04 130
drwxr-xr-x 3 cezary cezary 4096 paź  8 19:04 common
lrwxrwxrwx 1 cezary cezary    3 paź  8 19:04 current -> 130
```

Najpierw musimy zdobyć numer bloku w którym jest przechowany katalog.

```
debugfs:  blocks /home/cezary/snap/discord
2625667 
```

Następnie wyświetlić jego zawartość polecniem `bdump`.

```
debugfs:  bdump 2625667
0000  ce90 0a00 0c00 0102 2e00 0000 cd90 0a00  ................
0020  0c00 0202 2e2e 0000 cf90 0a00 0c00 0302  ................
0040  3133 3000 d090 0a00 1000 0602 636f 6d6d  130.........comm
0060  6f6e 0000 7d13 0a00 c00f 0707 6375 7272  on..}.......curr
0100  656e 7400 0000 0000 0000 0000 0000 0000  ent.............
0120  0000 0000 0000 0000 0000 0000 0000 0000  ................
*
7760  0000 0000 0000 0000 0c00 00de 38af f77f  ............8...
```
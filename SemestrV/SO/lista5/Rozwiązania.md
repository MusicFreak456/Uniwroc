# Systemy Operacyjne -- Lista 5
###### tags: `SO`

## Zadanie 1

### Co robi operacja read przy pustym buforze?

Próby odczytu z pustej rury są blokujące. Wybudzenie następuje kiedy cokolwiek jest zapisane do rury, lub gdy wejście rury zostanie zamknięte (wtedy odczytany jest koniec pliku)

Z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* [44.1]:

![](https://i.imgur.com/FRlIXVW.png)

### Co robi write przy pełnym buforze?

W zależności od tego czy chcemy zapisać więcej, czy mniej bajtów niż pojemność rury (`PIPE_BUF`), zachowanie może być różne

* zapisujemy mniej niż `PIPE_BUF` -- wywołanie jest blokujące i czeka aż będzie miejsce w rurze żeby wypisać dane na raz.
* zapisujemy więcej niż `PIPE_BUF` -- `write` zapisze tyle bajtów ile jest w stanie (zapełni rurę), następnie zablokuje się w oczekiwaniu na zwolnienie miejsca

Z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* [44.1]:

![](https://i.imgur.com/KEo8qCg.png)

### Wiele procesów piszących mniej niż SIZE_BUF

Jeśli kilka procesów chce zapisać jednocześnie liczbę bajtów mniejszą niż `SIZE_BUF` do rury, to mamy gwarancję że zapisy te będą atomowe (niepodzielne, jednostkowe, w jednym kroku), czyli zapisywane dane na pewno nie będą się przeplatać.

Z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* [44.1]:

![](https://i.imgur.com/RRkSbLb.png)

### Dlaczego shell nie musi interweniować przy umieraniu procesów w potokach?

Rozpatrzmy dwa przypadki:

* procesowi czytającemu umrze producent - wtedy przy próbie odczytania wartości z rury konsument zobaczy koniec pliku (EOF) i prawdopodobnie niedługo po tym umrze śmiercią naturalną.
* procesowi piszącemu umrze konsument - wtedy przy próbie zapisu producent otrzyma (od jądra) sygnał `SIGPIPE`. Może go oczywiście zignorować, ale wtedy zapis zagończy się błędem z kodem `EPIPE`.

Żeby ten system funkcjonował bardzo ważne jest żeby tylko jeden proces miał otwarty deskryptor do końca rury, ale o to zadba powłoka przy tworzeniu potoku.

Z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* [44.1]:

![](https://i.imgur.com/4XrxkZk.png)


### Kiedy wystąpi short count?

Przy `read` -- tylko jeśli nie ma i nie będzie więcej danych do odczytania (deskryptor do zapisu zostanie zamknięty)

Przy `write` -- tylko jeśli zostanie on przerwany obsługą sygnału. Z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* [44.1]:

![](https://i.imgur.com/6VS6smR.png)

### Jak można połączyć rodzica i dziecko rurą po uruchomieniu dziecka?

Można np. użyć nazwanej rury, czyli FIFO (utworzonej przez `mkfifo`). Tak utworzoną rurą można łączyć dowolne dwa procesy. Żeby uzyskać koniec do zapisu proces musi otworzyć plik z flagą tylko do zapisu i analogicznie w przypadku chęci czytania z rury.

## Zadanie 2

### Do czego służy ioctl?

Jest to interfejs stworzony w celu manipulowania własnościami urządzeń, do których odnosi się konkretny plik specjalny. Widzieliśmy go w akcji przy okazji zadań z poprzednich list, np. gdy pobieraliśmy rozmiary terminala: 

```c=
struct winsize size;
ioctl(0,TIOCGWINSZ, &size);
```

lub gdy wybieraliśmy nową grupę pierwszoplanową terminala:
```c=
struct termios *argp
ioctl(fd, TCGETS, argp)
```

### Parametry ioctl

Pierwszym jest oczywiście deskryptor pliku specjalnego.

Drugim jest polecenie sterownika (zazwyczaj gotowe makro) zakodowane na 32-óch bitach w następujący sposób (z ioccom.h):
```
/*
 * Ioctl's have the command encoded in the lower word, and the size
 * any in or out parameters in the upper word.  The high 3 bits of the
 * upper word are used to encode the in/out status of the parameter.
 *
 *	 31 29 28                     16 15            8 7             0
 *	+---------------------------------------------------------------+
 *	| I/O | Parameter Length        | Command Group | Command       |
 *	+---------------------------------------------------------------+
 */
```

Czyli na najmłodszch bitach zapisujemy kod grupy polecenia i samego polecenia, a na najwyższych kodujemy czy trzeci parametr jest do odczytu czy zapisu (I/O) i jego długość.

Trzeci parametr jest dowolnego typu i to co ma się w nim znaleźć jest zależne od wykonywanej operacji. Zazwyczaj jest to jakiś wskaźnik na edytowaną/pobieraną strukturę.


### Krytyka interfejsu plików

![](https://i.imgur.com/yHFIgLL.png)

Autor doszukuje się brzydoty tego interfejsu w fakcie, że model obiektowy w Uniksie jest zbyt słaby żeby pliki specjalne mogły w wygodny sposób udostępniać interfejs do konfigurowania swoich własności (np. przez "magiczne" atrybuty). Jedynym atrybutem pliku jest jego zawartość, a przetrzymywanie w niej metadanych mogłoby być zbyt zawodne (i prowadzić do niejednoznaczności, np. w tym które operacje powinny je przepisywać).

Doprowadziło to do bardzo przerośniętego interfejsu `ioctl` i `fctl`, w którym każdy sterownik musi dostarczyć zestawu makr opisujących możliwe operacje, które często są słabo udokumentowane i nieprzenoszalne.

### Znaczenie podanych operacji

#### DIOCEJECT

```
dkio.h 	67 #define DIOCEJECT _IOW('d', 112, int) /* eject removable disk */
```

Wysuwa urządzenie przenośne (dysk).

#### KIOCTYPE

```
kbio.h 	110 #define KIOCTYPE _IOR('k', 9, int) /* get keyboard type */
```

Pobiera typ klawiatury. Możliwe typy (z dokumentacji) to
```
KB_SUN3   Sun Type 3 keyboard
KB_SUN4   Sun Type 4 keyboard
KB_ASCII  ASCII terminal masquerading as keyboard
KB_PC     Type 101 PC keyboard
KB_USB    USB keyboard
```

#### SIOCGIFCONF

```
66 #define	SIOCGIFCONF	_IOWR('i', 38, struct ifconf)	/* get ifnet list */
```

Pobiera z gniazda listę struktur typu `ifnet`, czyli jedną ze struktur służących do obsługi interfejsu sieci.

## Zadanie 3

### Reprezentacja katalogów

![](https://i.imgur.com/PAlXdDB.png)

O katalogu można myśleć jak o liście wiązanej wpisów typu `dirent` które zawierają pole opisujące do którego i-węzła wpis się odnosi, rozmiar wpisu w bajtach, typ pliku (ta informacja jest powielana z inode'a dla wydajności), długość nazwy oraz samą nazwę. Wpis jest zatem zmiennej długości, ale rozmiar nazwy ma tą długość ograniczoną.

O wielkości wpisu można myśleć jak o względnym wskaźniku na kolejny wpis. Wpisy mogą mieć padding większy niż ten wynikający z długości nazwy, takie dodatkowe miejsce nazywa się nieużytkiem. Nie dopuszcza się także żeby pomiędzy wpisami znajdowała się pusta przestrzeń nienależąca do żadnego wpisu.

### Jak przebiega operacja usuwania pliku

![](https://i.imgur.com/TNRyrrb.png)

Przeszukujemy listę liniowo, aż natrafimy na wpis z szukanym przez nas plikiem. Jeśli go znajdziemy to w jego poprzedniku zwiększamy deklarowany rozmiar wpisu o rozmiar wpisu usuwanego, w ten sposób poprzednik zacznie wskazywać na następnika usuniętego pliku, a miejsce które po usuniętym pliku pozostanie zostanie wchłonięte przez poprzednika jako nadmiarowy padding, czyli nieużytek.

Obecność użytków przed lub po wpisie nie stanowi problemu, gdyż każdy nieużytek jest częścią jakiegoś wpisu.

Alternatywnie może używana być technika, w której usuwanemu wpisowi ustawiamy numer `inode` na 0, co oczywiście nie jest poprawną wartością, ale ma służyć sygnalizowaniu że wpis ten należy pominąć przy trawersowaniu.

### Operacja dodawania pliku

Poniższe wnioski zostały wyciągnięte z analizy kodu jądra systemu linux, a konretnie pliku o ścieżce `root/fs/ext2/dir.c` w archiwum `https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/`

Zaczynamy od liniowego przeszukiwania kolejnych wpisów, aż nie zajdzie jeden z warunków

* rozmiar paddingu (części nazywanej nieużytkiem) obecnego wpisu pozwala na umieszczenie w nim dodawanego wpisu -- dodajemy wpis za nazwą pliku w obecnie przeglądanym wpisie
* obecny wpis ma numer inode'a `0` (czyli jest nieużywany) i jego wielkość pozwala na zapis dodawanego wpisu. -- dodajemy wpis w tym miejscu
* dotarliśmy do końca katalogu -- dodajemy wpis w tym miejscu (prawdopodobnie będzie trzeba go rozszerzyć)
* obecny wpis ma długość `0` -- kończymy z błędem

### Kiedy opłaca się wykonywać operację kompaktowania?

Operacja usunięcia wpisu spomiędzy dwóch innych zostawia wolne miejsce nazwane nieużytkiem. Miejsce to może, ale nie musi zostać wykorzystane w przyszłości, dlatego po pewnym czasie możemy znaleźć się w sytuacji, w której w stosunku do miejsca zarezerwowanego na katalog, faktycznie używamy bardzo małej jego części. Może opłacać się wtedy przeprowadzenie operacji kompaktowania, czyli przesunięcia wszystkich wpisów na początek pamięci zarezerwowanej dla katalogu, z pominięciem nieużytków i ewentualne zwolnienie jej zwolnionej części.


## Zadanie 4

### Trawersowanie ścieżki

Ścieżkę bezwzględną można poznać po tym, że zaczyna się od `/` czyli od katalogu root'a. Dlatego tam rozpocznie się trawersowanie. 
```
cezary@cezary-MSI$ stat /
  Plik: /
  rozmiar: 4096         bloków: 8          bloki I/O: 4096   katalog
Urządzenie: 802h/2050d  inody: 2           dowiązań: 20
Dostęp: (0755/drwxr-xr-x)  Uid: (    0/    root)   Gid: (    0/    root)
Dostęp:      2021-11-07 21:08:12.586161987 +0100
Modyfikacja: 2011-10-12 09:32:33.000000000 +0200
Zmiana:      2021-11-01 11:44:20.030310637 +0100
Utworzenie:  -
```

Następnie sprawdzi wpisy w tym katalogu, w poszukiwaniu wpisu z nazwą `usr`, a z niego dowie się na jaki `inode` wskazuje. Wpisy w katalogu będą przeglądane liniowo.

```
cezary@cezary-MSI$ ls -lia / | grep usr
 [...]
 262145 drwxr-xr-x  14 root root  4096 lis  3 19:27 usr
```

Znany jest teraz kolejny `inode`. Proces ten będzie powtarzany aż dotrzemy do szukanego pliku. Po drodze sprawdzane są też oczywiście uprawnienia (czy użytkownik może odczytać dany katalog).

```
cezary@cezary-MSI$ ls -lia /usr | grep bin
262146 drwxr-xr-x   2 root root 69632 lis  7 19:56 bin
[...]
```

```
cezary@cezary-MSI$ ls -lia /usr/bin | grep cc
[...]
262329 lrwxrwxrwx  1 root root  20 paź  8 19:39 cc -> /etc/alternatives/cc
[...]
```

Teraz w zależności od tego, czy chcemy podążać za dowiązaniami symbolicznymi, możemy skończyć tutaj, albo rozwiązywać to dowiązanie.

```
cezary@cezary-MSI$ stat /usr/bin/cc
  Plik: /usr/bin/cc -> /etc/alternatives/cc
  rozmiar: 20           bloków: 0          bloki I/O: 4096   dowiązanie symboli.
Urządzenie: 802h/2050d  inody: 262329      dowiązań: 1
Dostęp: (0777/lrwxrwxrwx)  Uid: (    0/    root)   Gid: (    0/    root)
Dostęp:      2021-11-08 20:02:43.624936137 +0100
Modyfikacja: 2021-10-08 19:39:45.444177570 +0200
Zmiana:      2021-10-08 19:39:45.444177570 +0200
Utworzenie:  -
```

#### Jeśli podążamy za symlinkami

```
cezary@cezary-MSI$ ls -lia / | grep etc
1441793 drwxr-xr-x 157 root root 12288 lis  7 19:56 etc
```

```
cezary@cezary-MSI$ ls -lia /etc | grep alternatives
1441802 drwxr-xr-x   2 root root    12288 lis  1 11:43 alternatives
[...]
```

```
cezary@cezary-MSI$ ls -lia /etc/alternatives | grep cc
[...]
1442178 lrwxrwxrwx   1 root root    12 paź  8 19:39 cc -> /usr/bin/gcc
[...]
```

```
cezary@cezary-MSI$ ls -lia / | grep usr
[...]
262145 drwxr-xr-x  14 root root  4096 lis  3 19:27 usr
```

```
cezary@cezary-MSI$ ls -lia /usr | grep bin
262146 drwxr-xr-x   2 root root 69632 lis  7 19:56 bin
[...]
```

```
cezary@cezary-MSI$ ls -lia /usr/bin | grep gcc
[...]
262677 lrwxrwxrwx  1 root root   5 paź  8 19:39 gcc -> gcc-9
[...]
```

```
[...]
262678 lrwxrwxrwx  1 root root    22 paź  8 19:39 gcc-9 -> x86_64-linux-gnu-gcc-9
[...]
```

I w końcu docieramy do czegoś co jest faktycznie plikiem

```
[...]
263825 -rwxr-xr-x  1 root root     1154192 sie  8  2020 x86_64-linux-gnu-gcc-9
[...]
```

Dla potwierdzenia, że to faktycznie ten plik: 

```
cezary@cezary-MSI$ stat -L /usr/bin/cc
  Plik: /usr/bin/cc
  rozmiar: 1154192      bloków: 2256       bloki I/O: 4096   plik zwykły
Urządzenie: 802h/2050d  inody: 263825      dowiązań: 1
Dostęp: (0755/-rwxr-xr-x)  Uid: (    0/    root)   Gid: (    0/    root)
Dostęp:      2021-11-08 19:26:10.606772983 +0100
Modyfikacja: 2020-08-08 14:04:53.000000000 +0200
Zmiana:      2021-10-08 19:39:46.488126097 +0200
Utworzenie:  -
cezary@cezary-MSI$            
```

### Od jakiego numeru i-węzła algorytm zaczyna działanie?

Ścieżki bezwzględne zaczynają się w katalogu root, a dla niego zazwyczaj zarezerwowany jest inode, o numerze `2`.

### Skąd sterownik systemu pliku wie gdzie leży i-ty bajt?

Struktura `i-node` przechowuje w sobie wskaźniki na bloki danych powiązane z plikiem który reprezentuje:

![](https://i.imgur.com/5IGfnMe.png)

Znając wielkość pojedynczego bloku, możemy policzyć w którym bloku szukać i-tego bajtu. W zależności od wielkości pliku może zajść potrzeba odniesienia się do bloku niebezpośrednio.
Dzieje się tak, ponieważ struktura `i-node` przy dużej liczbie wskaźników na bloki przechowujące dane zaczyna używać wskaźników na bloki zawierające wskaźniki na bloki.

Organizacja wskaźników z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook*:

![](https://i.imgur.com/F8FIZvi.png)

### Dlaczego nie można tworzyć twardych dowiązań pomiędzy systemami plików?

Polecenie `ln /proc/version test` faktycznie zakończy się błędem `EXDEV`:

```
cezary@cezary-MSI$ ln /proc/version test
ln: nie udało się utworzyć dowiązania zwykłego 'test' => '/proc/version': 
Błędne dowiązanie między urządzeniami
```

Dzieje się tak ponieważ próbujemy utworzyć twarde dowiązanie pomiędzy plikami z dwóch różnych systemów plików, a nie jest to możliwe, ze względu na to, że i-węzły numerowane są niezależnie w obrębie jednego systemu, więc w dwóch różnych systemach mogą jednocześnie węzły o tym samym numerze, czyli utworzenie dowiązania do jednego z nich sprawiłoby że nie byłoby ono jednoznaczne. (powołując się na *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* [18.1])

![](https://i.imgur.com/Ev4jxGh.png)

## Zadanie 5

### Iteracja po wpisach

W polu `d_reclen` ze struktury `linux_diren` znajduje się rozmiar wpisu w bajtach, możemy to wykorzystać do przeskakiwania po kolejnych wpisach.

```c=
/* TODO: Iterate over directory entries and call file_info on them. */
for (int offset = 0; offset < n;){
  d = (struct linux_dirent *)(buf + offset);
  char *name = d->d_name;
  file_info(dirfd, name);
  offset += d->d_reclen;
}
```

### Pobranie metadanych

Użyjemy procedury `fstatat`, jedyną rzeczą na którą trzeba zwrócić uwagę to podanie opcji `AT_SYMLINK_NOFOLLOW`, żeby w przypadku plików które są dowiązaniami symbolicznymi, przeczytać ich metadane, a nie pliku na który wskazują.

```c=
/* TODO: Read file metadata. */
Fstatat(dirfd, name, sb, AT_SYMLINK_NOFOLLOW);
```

Struktura otrzymanych metadanych:
```c=
struct stat {
   dev_t     st_dev;         /* ID of device containing file */
   ino_t     st_ino;         /* Inode number */
   mode_t    st_mode;        /* File type and mode */
   nlink_t   st_nlink;       /* Number of hard links */
   uid_t     st_uid;         /* User ID of owner */
   gid_t     st_gid;         /* Group ID of owner */
   dev_t     st_rdev;        /* Device ID (if special file) */
   off_t     st_size;        /* Total size, in bytes */
   blksize_t st_blksize;     /* Block size for filesystem I/O */
   blkcnt_t  st_blocks;      /* Number of 512B blocks allocated */
   struct timespec st_atim;  /* Time of last access */
   struct timespec st_mtim;  /* Time of last modification */
   struct timespec st_ctim;  /* Time of last status change */
};
```


### Drukowanie rozmiaru lub pary major * minor

`major` definiuje klasą urządzenia, `minor` definiuje instancję. Można je pobrać używając odpowiednich makr. Jeśli to nie urządzenie, drukujemy rozmiar pliku.

```c=
/* TODO: For devices: print major/minor pair; for other files: size. */
if(S_ISBLK(sb->st_mode) || S_ISCHR(sb->st_mode)){
  printf(" %5u,%5u", major(sb->st_rdev), minor(sb->st_rdev));
}
else {
  off_t f_size = sb->st_size;
  printf(" %11ld", f_size);
}
```

### Odczytanie zawartości symlinka

```c=
if (S_ISLNK(sb->st_mode)) {
/* TODO: Read where symlink points to and print '-> destination' string. */
  char symlink_value[PATH_MAX];
  size_t b_read = Readlinkat(dirfd, name, symlink_value, PATH_MAX);
  symlink_value[b_read] = '\0';
  printf(" -> %s", symlink_value);
}
```

### Sticky, set-uid, set-gid

`sticky` - informuje że pliki umieszone w katalogu mogą być skasowane tylko przez właściciela, albo użytkownika z prawami do zapisu do tego katalogu.

`set-gid` - plik utworzony z ustawionym tym bitem ma przynależność do grupy katalogu w którym się znajduje, a nie na główną grupę użytkownika który go utworzył.

`set-uid` - plik zostanie uruchomiony z prawami użytkownika, który jest właścicielem pliku, a nie użytkownika który go uruchomił.

Wytłumaczenie oznaczeń: https://expertlinux.eu/2016/03/29/co-to-jest-sticky-bit-suid-i-sgid/

```c=
/* TODO: Fix code to report set-uid/set-gid/sticky bit as 'ls' does. */
ux = (m & S_ISGID) ? ( (ux == 'x' ? 's' : 'S') ) : ux;
gx = (m & S_ISUID) ? ( (gx == 'x' ? 's' : 'S') ) : gx;
ox = (m & S_ISVTX) ? ( (ox == 'x' ? 't' : 'T') ) : ox;
```

## Zadanie 6

```c=
static noreturn void filter_chain(pipe_t in) {
  long prime;

  if(!(ReadNum(in, &prime))){
    CloseReadEnd(in);
    while(waitpid(-1, NULL, 0) > 0);
    exit(EXIT_SUCCESS);
  }
  printf("%ld\n", prime);

  pipe_t pass_pipe = MakePipe();

  if(Fork()){
    CloseReadEnd(in);
    CloseWriteEnd(pass_pipe);
    filter_chain(pass_pipe);
  } else {
    CloseReadEnd(pass_pipe);
    filter(in, pass_pipe, prime);
    CloseWriteEnd(pass_pipe);
    CloseReadEnd(in);
  }

  exit(EXIT_SUCCESS);
}
```

## Zadanie 7

```c=
static void Sort(int parent_fd) {
  int nelem = ReadNum(parent_fd);

  if (nelem < 2) {
    WriteNum(parent_fd, ReadNum(parent_fd));
    Close(parent_fd);
    return;
  }

  sockpair_t left = MakeSocketPair();
  /* TODO: Spawn left child. */
  if(Fork()){
    Close(left.child_fd);
  }
  else {
    Close(parent_fd);
    Close(left.parent_fd);
    Sort(left.child_fd);
    exit(EXIT_SUCCESS);
  }

  sockpair_t right = MakeSocketPair();
  /* TODO: Spawn right child. */
    if(Fork()){
    Close(right.child_fd);
  }
  else {
    Close(parent_fd);
    Close(left.parent_fd);
    Close(right.parent_fd);
    Sort(right.child_fd);
    exit(EXIT_SUCCESS);
  }

  /* TODO: Send elements to children and merge returned values afterwards. */
  int left_nelem = nelem / 2;
  int right_nelem = nelem - left_nelem;

  SendElem(parent_fd, left.parent_fd, left_nelem);
  SendElem(parent_fd, right.parent_fd, right_nelem);

  Merge(left.parent_fd, right.parent_fd, parent_fd);
  
  Close(parent_fd);
  Close(left.parent_fd);
  Close(right.parent_fd);

  /* Wait for both children. */
  Wait(NULL);
  Wait(NULL);
}
```
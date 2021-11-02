# Systemy Operacyjne -- Lista 4
###### tags: `SO`

## Zadanie 1

Prześledzimy kod procedury `tty_curpos` zapisany w `terminal.c` (najpierw kod, później opis, tak, wiem, to dezorientuje, w trzecim się ogarnę)

```c=
void tty_curpos(int fd, int *x, int *y) {
```
Procedura przyjmuje deskryptor pliku odnoszący się do terminala, oraz dwa wskaźniki przez które zwrócona zostanie para współrzędnych opisujących pozycję kursora.

```c=
  struct termios ts, ots;

  tcgetattr(fd, &ts);
```

`tcgetattr` pobiera spod danego deskryptora informacje o terminalu znajdującym się pod nim i zwraca je przez strukturę typu `termios`, która przechowuje własności terminala. Zdefiniowana jest ona następująco

![](https://i.imgur.com/PfGoNdW.png)

```c=
  memcpy(&ots, &ts, sizeof(struct termios));
  ts.c_lflag &= ~(ECHO | ICANON);
  ts.c_cflag &= ~CREAD;
  tcsetattr(fd, TCSADRAIN, &ts);
```

W kolejnym kroku tworzymy kopię `ts` i umieszczamy ją w `ots`, a następnie ustawiamy w niej pewne flagi i aktualizujemy własności terminala poleceniem `tcsetattr`. Drugi argument tej procedury definiuje moment, w którym zmiany mają wejść w życie, możliwe opcje to:

* `TCSANOW` - zmiany wchodzą w życie natychmiast
* `TCSADRAIN` - wchodzą w życie kiedy obecnie zakolejkowane wyjście zostanie wypisane
* `TCSAFLUSH` - tak samo jak `TCSADRAIN`, ale zakolejkowane wejście jest czyszczone przy wprowadzaniu zmian

Zmienione przez nas flagi mają następujące znaczenie:

* `ECHO` - włącza/wyłącza echo (dopisywanie wejścia do wyjścia)
* `ICANON` - włącza/wyłącza przetwarzanie w trybie kanonicznym
* `CREAD` - włącza/wyłącza odbiornik, jeśli jest wyłączony to żaden znak nie zostanie wczytany

```c=
  /* How many characters in the input queue. */
  int m = 0;
  /* TODO: Need to figure out some other way to do it on MacOS / FreeBSD. */
#ifdef LINUX
  ioctl(fd, TIOCINQ, &m);
#endif

  /* Read them all. */
  char discarded[m];
  m = Read(fd, discarded, m);
```
Kolejnym krokiem jest wczytanie wszystkich znaków znajdujących się w kolejce wejściowej sterownika terminala, aby to co zapiszemy w następnej kolejności zostało obsłużone jako pierwsze. Liczbę znaków w kolejce zdobywamy za pomocą polecenia `ioctl` oraz opcji `TIOCINQ`, która właśnie to robi.

```c=
  Write(fd, CPR(), sizeof(CPR()));
```

Wypisujemy na wyjście kod strerujący CPR (*Cursor Position Report*), który wyznacza pozycję kursora i ogłasza ją wypisując `CSIx;yR`, gdzie `x` to liczba wierszy, a `y` kolumn.

```c=
  char buf[20];
  int n = Read(fd, buf, 19);
  buf[n] = '\0';
```
Teraz odczytujemy wynik wypisania znaku CPR.

```c=
  ts.c_lflag |= ICANON;
  tcsetattr(fd, TCSADRAIN, &ts);
  for (int i = 0; i < m; i++)
    ioctl(fd, TIOCSTI, discarded + i);
```

Ponownie w trybie kanonicznym, dodajemy wszystkie znaki do kolejki wejściowej (bo to robi opcja `TIOCSTI`), które z niej wcześniej tymczasowo wyciągnęliśmy.

```c=
  tcsetattr(fd, TCSADRAIN, &ots);
```

Przywracamy początkowe własności terminala.

```c=
  sscanf(buf, "\033[%d;%dR", x, y);
}
```

Sczytujemy współrzędne kursora z wyniku wypisania CPR.

### ioctl z opcjami TCGETS, TCSETSW

Tak jak jest to napisane w poleceniu,`ioctl` z tymi opcjami zrobi to samo co odpowiednio `tcgetattr` i `tcsetattr`(z opcją `TCSADRAIN`). Powód istnienia tych funkcji owijających jest uzasadniony brakiem możliwości sprawdzenia typów ostatniego argumentu w `ioctl`.

![](https://i.imgur.com/Nx4c0Uk.png)


## Zadanie 2

Program script podczepia się pomiędzy terminal, a (domyślnie) nowo utworzoną instancję shella (można to zmienić flagą `-c`). Tworzy przy tym pseudoterminal, którego slave'a otrzyma nowa instancja shella. Wszystko co zostanie zapisane na końcówkę slave, zostanie skopiowane to pliku (domyślnie pliku o nazwie `typescript`). Typowo znajdą się tam też wpisywane polecenia, gdyż to co użytkownik pisze do shell'a jest mu wyświetlane (przy włączonym echo, czyli np. hasła nie powinny zostać zapisane).

![](https://i.imgur.com/0tZ7gJN.png)

### Przykładowa interakcja z programem

Polecenia różnią się od tych z polecenia, gdyż posiadam starszą wersję programu `script`.

![](https://i.imgur.com/V6Xd0jy.png)

### Wydruk polecenia strace

Flaga `-f`, sprawia że śledzone będą także procesy potomne, a flaga `-e` specyfikuje, które wywołania mają być śledzone.

Przykładowa interakcja `script` z `ls` odpalonym pod `dashem` poprzez pseudoterminal wygląda tak (pierwsza kolumna to `pid`):
```
11108 write(1, "script.log  timing  typescript\n", 31) = 31
10926 read(3, "script.log  timing  typescript\r\n", 8192) = 32
10926 write(1, "script.log  timing  typescript\r\n", 32) = 32
```

Widać, że to co wypisze `ls` (11108) (który odziedziczył deskryptory po dashu) jest odczytywane z mastera przez `script` (10926), a następnie przekazywane na jego własne wyjście. To czego tu nie widzimy to że do bufora który zostanie wypisany do `typescript` i `timing` również dodawane są wpisy. Zostaną one wypisane po zakończeniu działania dasha:

```
10926 write(1, "Skrypt wykonany, plik to typescr"..., 36) = 36
10926 write(7, "0.016407 2\n3.396504 1\n0.220045 1"..., 157) = 157
10926 write(6, "Script started on 2021-11-01 22:"..., 226) = 226
```

Możemy podejrzeć do czego odnoszą się deskryptory poprzez system `/proc`. Wyświetlają się one jako symlinki na ścieżce `/proc/<pid>/fd/`. Przykład takiego podglądnięcia dla `script`:

![](https://i.imgur.com/6o6E1lV.png)

### Dowód, że sterownik przepisuje wejścia zgodnie z flagami

`ICRNL` - przepisując wejście, zamień `/r` na `/n`

Tą sytuację możemy zaobserwować kiedy kończymy wpisywać polecenie poprzez `script` na mastera i `dash` podnosi je ze swojego sleve'a:

```
10927 read(0,  <unfinished ...>
...
10926 read(0, "\r", 8192)               = 1
10926 write(3, "\r", 1)                 = 1
10927 <... read resumed>"ls\n", 8192)   = 3
```

`ONLCR` - przepisując wyjście, zamień `/n` na `/r/n`

To zjawisko można zaobserwować w podanym wcześniej przykładzie przepisywania wyjścia `ls`:

```
11108 write(1, "script.log  timing  typescript\n", 31) = 31
10926 read(3, "script.log  timing  typescript\r\n", 8192) = 32
10926 write(1, "script.log  timing  typescript\r\n", 32) = 32
```

## Zadanie 3

Skoro mamy skupić się tylko na kilku wywołaniach, to `dasha` uruchomimy pod kontrolą `strace` z wyspecyfikowanymi wywołaniami, które chcemy śledzić

```shell=
strace -e read,write,openat,dup2,pipe,close,clone,execve -o pipeline.log -f dash 
```

Wywołania (te ciekawsze) mają następujące znaczenie:

* `openat` - działa tak samo jak open, ale jeśli ścieżka jest względna to zostanie zinterpretowana względem katalogu, którego deskryptor podajemy w pierwszym argumencie. Podanie `AT_FDCWD` oznacza interpretowanie względem bieżącego katalogu procesu.
* `dup2` - klonuje deskryptor pliku. Można wyspecyfikować źródłowy i docelowy.
* `pipe` - tworzy nową rurę i zwraca przez argument parę deskryptorów plików identyfikujących jej końce. Pierwszy to jej wylot.
* `clone` - podobny do fork, ale pozwala na precyzyjną kontrolę nad elementami współdzielonymi z rodzicem.

`Read` i `write` dopisałem żeby wiedzieć co się dzieje.

### Wydruk pipeline.log

(polecam "znajdź i zamień" pidów na nazwy procesów)

Prześledzimy co się dzieje po wczytaniu polecenia przez powłokę `dash` (najpierw opis, później kod):

```
<dash> write(2, "$ ", 2) = 2
<dash> read(0, "ps -ef | grep sh | wc -l > cnt\n", 8192) = 31
```

W pierwszej kolejności tworzymy świeżą rurę, która znajdzie się między `ps`, a `grep`. Jej wejście to deskryptor `4`, a wyjście to `3`. Następnie forkujemy się do nowego procesu, który wkrótce stanie się `ps`'em.

```
<dash> pipe([3, 4])                      = 0
<dash> clone(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7fed69157850) = <ps>
```

W nowo utworzonym procesie (oznaczonym `ps`) zamykamy deskryptor 3, gdyż nie będziemy w nim używać wyjścia rury, natomiast jej wejście przepinamy pod standardowe wyjście oraz wołamy `execve` żeby uruchomić już faktycznie program `ps`.

```
<ps> close(3 <unfinished ...>
[...]
<ps> <... close resumed>)              = 0
[...]
<ps> dup2(4, 1)                        = 1
<ps> close(4)                          = 0
[...]
<ps> execve("/usr/bin/ps", ["ps", "-ef"], 0x55a58853a238 /* 60 vars */ <unfinished ...>
<ps> <... execve resumed>)             = 0
```

W międzyczasie dash kontynuuje łaczenie procesów rurami, tym razem tworzy tą która zostanie umieszczona między `grep` a `wc`. Jej wejście będzie w deskryptorze 5, a wyjście w 4. Następnie `dash` znowu się forkuje do nowego procesu (grepa).

```
<dash> pipe( <unfinished ...>
[...]
<dash> <... pipe resumed>[4, 5])         = 0
[...]
<dash> clone(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD <unfinished ...>
[...]
<dash> <... clone resumed>, child_tidptr=0x7fed69157850) = <grep>
```

Nowo utworzony grep poprzepina odpowiednie końce rur do swojego standardowego wejścia i wyjścia, następnie wymieni sobie mózg.

```
<grep> close(4 <unfinished ...>
[...]
<grep> <... close resumed>)              = 0
[...]
<grep> dup2(3, 0 <unfinished ...>
[...]
<grep> <... dup2 resumed>)               = 0
<grep> close(3 <unfinished ...>
[...]
<grep> <... close resumed>)              = 0
[...]
<grep> dup2(5, 1)                        = 1
[...]
<grep> close(5)                          = 0
<grep> execve("/usr/bin/grep", ["grep", "sh"], 0x55a58853a268 /* 60 vars */ <unfinished ...>
[...]
<grep> <... execve resumed>)             = 0
```

`dash` pozamyka już niepotrzebne mu deskryptory i będzie kontynuował swoje żmudne, niczym to zadanie, łączenie kolejnych procesów.

```
<dash> close(3)                          = 0
<dash> close(5)                          = 0
```

Jedyna ciekawa rzecz która się zdarzy później, to kiedy wyjściem `wc` musi zostać plik `cnt`. W tym celu w sforkowanym procesie przeznaczonym dla `wc`, zanim wykonane zostanie `execve`, standardowe wyjście podmieniane jest na deskryptor otwartego pliku poleceniem `openat`.

```
<wc> dup2(4, 0)                        = 0
[...]
<wc> close(4)                          = 0
[...]
<wc> openat(AT_FDCWD, "cnt", O_WRONLY|O_CREAT|O_TRUNC, 0666 <unfinished ...>
[...]                     = 0
<wc> <... openat resumed>)             = 3
<wc> close(1)                          = 0
<wc> dup2(3, 1)                        = 1
<wc> close(3 <unfinished ...>
[...]
<wc> <... close resumed>)              = 0
[...]
<wc> execve("/usr/bin/wc", ["wc", "-l"], 0x55a58853a2b8 /* 60 vars */ <unfinished ...>
[...]
<wc> <... execve resumed>)             = 0
```

## Zadanie 4

### Wydruk pipeline.log

#### Umieszczenie dzieci w grupach

Skoro mamy skupić się tylko na kilku wywołaniach, to `dasha` uruchomimy pod kontrolą `strace` z wyspecyfikowanymi wywołaniami, które chcemy śledzić

```shell=
strace -e read,write,clone,execve,setpgid,getpgid -o pipeline.log -f dash
```

`Read` i `write` dopisałem żeby wiedzieć co się dzieje.

```
30153 read(0, "ps -ef | grep sh | wc -l > cnt\n", 8192) = 31
30153 clone(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7fe6bcc6c850) = 30157
30153 setpgid(30157, 30157)             = 0
30157 setpgid(0, 30157)                 = 0
30153 clone(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7fe6bcc6c850) = 30158
30153 setpgid(30158, 30157)             = 0
30158 setpgid(0, 30157)                 = 0
30157 execve("/usr/bin/ps", ["ps", "-ef"], 0x5631a9a3a238 /* 60 vars */ <unfinished ...>
30153 clone(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7fe6bcc6c850) = 30159
30157 <... execve resumed>)             = 0
30153 setpgid(30159, 30157)             = 0
30159 setpgid(0, 30157)                 = 0
30158 execve("/usr/bin/grep", ["grep", "sh"], 0x5631a9a3a268 /* 60 vars */) = 0
30157 read(3, "\177ELF\2\1\1\0\0\0\0\0\0\0\0\0\3\0>\0\1\0\0\0 K\0\0\0\0\0\0"..., 832) = 832
30159 execve("/usr/bin/wc", ["wc", "-l"], 0x5631a9a3a2b8 /* 60 vars */) = 0
```

Widać że po każdym forkowaniu następują dwa wywołania `setpgid`. Jedno w rodzicu, w którym zarówno pidem procesu, któremu zmieniamy grupę jak i id grupy jest pid dziecka, a następnie drugie w dziecku, w którym argumenty są takie same, bo 0 oznacza że proces zmienia swoją grupę.

Potrzeba dwóch wywołań wynika z tego, że procesowi, który już wykonał `execve` nie da się zmienić grupy z poziomu rodzica, więc nie moglibyśmy tego robić tylko w procesie `shella`, bo dziecko mogłoby już wykonać `execve` (nie wiemy w jakiej kolejności się wykonają). Tak samo nie możemy tego zrobić tylko w dziecku, bo nie wiemy ile czasu minie zanim proces dziecka otrzyma czas procesora żeby to zrobić i w międzyczasie mógłby do grupy docelowej zostać wysłany sygnał.

Kod zapobiegający temu wyścigowi będzie miał strukturę podobną do poniższego:

![](https://i.imgur.com/thtjarn.png)

### Wybór grupy pierwszoplanowej 

Polecenie:

```shell=
strace -e read,write,clone,execve,setpgid,getpgid,ioctl -o pipeline.log -f dash
```

Powłoka wybiera grupę pierwszoplanową terminala w dwóch miejscach.

* Po uruchomieniu, ustawia ją na własną grupę
```
32984 execve("/usr/bin/dash", ["dash"], 0x7ffe6d4d56f8 /* 60 vars */) = 0
[...]
32984 ioctl(0, TCGETS, {B38400 opost isig icanon echo ...}) = 0
32984 ioctl(1, TCGETS, {B38400 opost isig icanon echo ...}) = 0
32984 ioctl(10, TIOCGPGRP, [32981])     = 0
32984 setpgid(0, 32984)                 = 0
32984 ioctl(10, TIOCSPGRP, [32984])     = 0
```
* Po zakończeniu działania procesów z grupy pierwszoplanowej, tak samo ustawia ją na swoją
```
32984 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=32987, si_uid=1000, si_status=0, si_utime=0, si_stime=3} ---
32988 +++ exited with 0 +++
32984 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=32988, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
32989 +++ exited with 0 +++
32984 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=32989, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
32984 ioctl(10, TIOCSPGRP, [32984])     = 0
```

Po sforkowaniu pierwszoplanowego dziecka, sam wykona on ustawienie swojej grupy jako pierwszoplanowej (nie robi tego powłoka) np.
```
32984 clone(child_stack=NULL, flags=CLONE_CHILD_CLEARTID|CLONE_CHILD_SETTID|SIGCHLD, child_tidptr=0x7fc7cfa18850) = 32987
32984 setpgid(32987, 32987)             = 0
32987 setpgid(0, 32987)                 = 0
32987 ioctl(10, TIOCSPGRP, [32987])     = 0
[...]
32987 execve("/usr/bin/ps", ["ps", "-ef"], 0x55c5e5208238 /* 60 vars */ <unfinished ...>
```

### W jaki sposób wyznaczany jest kod wyjścia?

Na podstawie dokumentacji:

![](https://i.imgur.com/ojlGzjF.png)

Kodem wyjścia potoku będzie kod wyjścia ostatniego polecenia w potoku. Kody wyjścia są zczytywane poprzez polecenie `wait4`.

```
33872 +++ exited with 0 +++
<dash> <... wait4 resumed>[{WIFEXITED(s) && WEXITSTATUS(s) == 0}], WSTOPPED, NULL) = 33872
33874 <... close resumed>)              = 0
33873 +++ exited with 0 +++
<dash> --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=33872, si_uid=1000, si_status=0, si_utime=0, si_stime=2} ---
33874 close(2 <unfinished ...>
<dash> rt_sigreturn({mask=[]} <unfinished ...>
33874 <... close resumed>)              = 0
<dash> <... rt_sigreturn resumed>)       = 33872
33874 exit_group(0 <unfinished ...>
<dash> --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=33873, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
33874 <... exit_group resumed>)         = ?
<dash> rt_sigreturn({mask=[]})           = 33872
<dash> wait4(-1, [{WIFEXITED(s) && WEXITSTATUS(s) == 0}], WSTOPPED, NULL) = 33873
<dash> wait4(-1,  <unfinished ...>
33874 +++ exited with 0 +++
<dash> <... wait4 resumed>[{WIFEXITED(s) && WEXITSTATUS(s) == 0}], WSTOPPED, NULL) = 33874
<dash> --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=33874, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
<dash> rt_sigreturn({mask=[]})           = 33874
<dash> ioctl(10, TIOCSPGRP, [<dash>])     = 0
<dash> wait4(-1, 0x7ffd25d38d5c, WNOHANG|WSTOPPED, NULL) = -1 ECHILD (Brak procesów potomnych)
```

## Zadanie 5

### Dlaczego nie można używać read i write na katalogu?

Na wykładzie zostało to wytłumaczone tym, że katalogi to pliki ze ścisłą strukturą i muszą być zapisywane i odczytywane całymi rekordami, w przeciwnym wypadku można by łatwo naruszyć strukturę jednego, albo kilku rekordów nadpisując je. (i lepszego wytłumaczenia nie znalazłem)

Wpis w katalogu ma następującą strukturę:
```c=
struct dirent {
     ino_t          d_ino;       /* Inode number */
     off_t          d_off;       /* Not an offset; see below */
     unsigned short d_reclen;    /* Length of this record */
     unsigned char  d_type;      /* Type of file; not supported
                                    by all filesystem types */
     char           d_name[256]; /* Null-terminated filename */
 };
```

najistotniejsze pola to `d_ino` identyfikujący strukturę Inode pliku oraz `d_name` nazywający ten plik w tym katalogu. Jeden plik może mieć wiele takich dowiązań (hard linków), przez co może mieć również wiele nazw. Struktura inode będzie przechowywać liczbę tych dowiązań i plik zostanie zwolniony dopiero gdy ich liczba spadnie do 0.

## Zadanie 6

Rozpatrujemy poniższy kod:

```c=
#include "csapp.h"

bool f_lock(const char *path) {
  if (access(path, F_OK) == 0)
    return false;
  (void)Open(path, O_CREAT|O_WRONLY, 0700);
    return true;
}

void f_unlock(const char *path) {
  Unlink(path);
}
```

implementuje on zakładanie blokady międzyprocesowej, której idea polega na tym, że jeśli plik jest już utworzony, to znaczy że blokada jest już założona przez inny proces, w przeciwnym wypadku można ją założyć poprzez utworzenie pliku. Usuwanie blokady polega na usunięciu odpowiedniego pliku.

Użyte funkcje mają następującą semantykę:

* `access` - sprawdza jakie uprawnienia ma użytkownik w stosunku do podanego pliku. Drugi argument to maska mówiąca, jakiego rodzaju weryfikacja mają zostać wykonane. W tym przypadku opja `F_OK` sprawdzi czy plik istnieje.
* `open` - otwiera plik. Znaczenie użytych opcji:
  * `O_CREAT` - jeśli plik nie istnieje to tworzy go jako plik zwykły
  * `O_WRONLY` - otwiera plik w trybie tylko do odczytu
* `unlink` - usuwa referencję do pliku o podanej nazwie i jeśli było to ostatnie odwołanie to usuwa plik

### Problem w tym kodzie

Występuje w nim problem typu **TOCTTOU** *(Time Of Check To Time Of Use)*, spowodowany założeniem, że między sprawdzeniem stanu pliku, a wykonaniem na nim operacji nie nastąpiła żadna zmiana. Co oczywiście nie musi być prawdziwe w środowisku w którym wiele procesów działa współbieżnie.

W tym konkretnym przypadku między wywołaniem procedury `access`, w celu sprawdzenia czy plik istnieje, a `Open`, jeśli nie istnieje, jest wystarczająco czasu żeby inny proces utworzył taki plik, gdyż nie są one wykonywane atomowo.

Chcemy zatem żeby sprawdzenie czy plik istnieje oraz jego utworzenie były wykonywane niepodzielnie. Z pomocą przychodzi nam opcja `O_EXCL`. Screen z *APUE*:

![](https://i.imgur.com/scFS5jK.png)

Przerobiony kod tak by używał tej flagi, będzie wyglądać tak:

```c=
#include "csapp.h"

bool f_lock(const char *path) {
  if (Open(path, O_CREAT|O_WRONLY|O_EXCL, 0700) < 0)
    return false;
  else
    return true;
}

void f_unlock(const char *path) {
  Unlink(path);
}
```

## Zadanie 7

### Innocent

```c=
#include "csapp.h"

int main(void) {
  long max_fd = sysconf(_SC_OPEN_MAX);
  int out = Open("/tmp/hacker", O_CREAT | O_APPEND | O_WRONLY, 0666);

  for (long i = 3; i < max_fd; i++)
  {
    if(i == out) continue;
    
    char symlink_path[14 + 19 + 1];
    sprintf(symlink_path,"/proc/self/fd/%ld",i);
    
    if(faccessat(-1, symlink_path, F_OK, AT_SYMLINK_NOFOLLOW) < 0) continue;

    char symlink_value[PATH_MAX];
    Readlink(symlink_path, symlink_value, PATH_MAX);

    dprintf(out, "\nFile descriptor %ld is '%s' file!\n", i, symlink_value);

    off_t old_pos;
    if((old_pos = lseek(i, 0, SEEK_CUR)) == -1) continue;
    Lseek(i, 0, SEEK_SET);

    char buff[BUFSIZ];
    size_t read;
    while ((read = Read(i, buff, BUFSIZ)))
    {
      Write(out, buff, read);
    }
    
    Lseek(i, old_pos, SEEK_SET);
  }
  

  Close(out);

  printf("I'm just a normal executable you use on daily basis!\n");

  return 0;
}

```

### Leaky

```c=
#include "csapp.h"

int main(int argc, char **argv) {
  long max_fd = sysconf(_SC_OPEN_MAX);

  /* Initialize PRNG seed. */
  struct timeval tv;
  gettimeofday(&tv, NULL);
  srandom(tv.tv_usec);

  /* This opens a file with password that is checked later. */
  int fd_1 = Open("mypasswd", O_RDONLY, 0);
  int fd_2 = 3 + random() % (max_fd - 3);
  (void)Dup2(fd_1, fd_2);
  Close(fd_1);
  Lseek(fd_2, 0, SEEK_END);

  // Żeby poprawić:
  // fcntl(fd_2, F_SETFD, FD_CLOEXEC);

  /* Let's suppose a user typed in correct password and was allowed to execute
   * a command and they choose to run our program. */
  int rc = system("./innocent");
  if (rc < 0)
    unix_error("System error");

  /* At this point we may finally close the file. */
  Close(fd_2);

  return rc;
}

```

## Zadanie 8

### Co robi program mkholes

```c=
#include "csapp.h"

#define BLKSZ 4096 /* block size */
#define NBLKS 8192 /* number of blocks written to a file */
#define WRAP 64

int main(void) {
  int fd = Open("holes.bin", O_CREAT | O_TRUNC | O_WRONLY, 0644);

  int usedblks = 0;

  for (int i = 0; i < NBLKS; i++) {
    if (i % WRAP == 0)
      dprintf(STDERR_FILENO, "%04d ", i);
    if (random() % 64) {
      Lseek(fd, BLKSZ, SEEK_CUR);
      Write(STDERR_FILENO, ".", 1);
    } else {
      char blk[BLKSZ];
      for (int j = 0; j < BLKSZ; j++)
        blk[j] = random();
      Write(fd, blk, BLKSZ);
      Write(STDERR_FILENO, "O", 1);
      usedblks++;
    }
    if (i % WRAP == WRAP - 1)
      Write(STDERR_FILENO, "\n", 1);
  }
  Close(fd);

  dprintf(STDERR_FILENO, "Non-zero blocks: %d\n", usedblks);

  return 0;
}
```

Otwiera plik `holes.bin`, następnie zapisuje do niego `NBLKS` bloków, losowo wybierając czy dany blok będzie niepusty i będzie zawierał losowe dane, czy będzie pusty. W przypadku bloku pustego wykorzystana jest technika tworzenia dziur, czyli kursor pliku jest przesuwany na pozycję większą niż rozmiar pliku. Przestrzeń między końcem pliku, a pozycją kursora umiejscowioną dalej, w której po raz pierwszy zostanie wykonana operacja `write`, jest pusta i nie trzeba faktycznie alokować bloków pamięci na dysku żeby ją przechowywać. Tą pustą przestrzeń nazywa się *dziurą* i każdy odczytany bajt z niej będzie miał wartość `0`.

### Wydruk polecenia stat

```
Napisz „stat --help” dla uzyskania informacji.
cezary@cezary-MSI$ stat holes.bin
  Plik: holes.bin
  rozmiar: 33550336     bloków: 1112       bloki I/O: 4096   plik zwykły
Urządzenie: 802h/2050d  inody: 1722508     dowiązań: 1
Dostęp: (0644/-rw-r--r--)  Uid: ( 1000/  cezary)   Gid: ( 1000/  cezary)
Dostęp:      2021-11-01 10:31:50.722027313 +0100
Modyfikacja: 2021-11-01 10:31:50.582026315 +0100
Zmiana:      2021-11-01 10:31:50.582026315 +0100
Utworzenie:  -

```

Wydrukowane zostają pola struktury zdefiniowanej następujuąco

```
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

### Wyliczony rozmiar

Jeden blok I/O to 4096 bajtów, pole `st_blocks` mówi nam o blokach 512 bajtowych, więc wartość tego pola będziemy dzielić przez 8, czyli bloków 4096 bajtowych mamy `1112 / 8 = 139`.

Rozmiar wynikający z liczby bloków i ich rozmiaru to

$$
139 \cdot 4096 = 569344
$$

Liczba bloków wynikająca z pola `st_size`:

$$
  33550336 \div 4096 = 8191
$$

#### Dlaczego liczba faktycznych bloków jest mniejsza od wynikającej z st_size?

Tak jak opisane to było wyżej, plik posiada dziury, na które nie trzeba alokować dodatkowych bloków pamięci.

#### Dlaczego liczba alokowanych bloków jest większa od deklarowanej przez mkholes?

Nie jestem pewien, aczkolwiek mam tezę:
Dzieje się tak, ponieważ struktura `i-node` przy dużej liczbie wskaźników na bloki przechowujące dane zaczyna używać wskaźników na bloki zawierające wskaźniki na bloki.

Organizacja systemu plików (z APUE):
![](https://i.imgur.com/zPaQH49.png)

![](https://i.imgur.com/5IGfnMe.png)

Organizacja wskaźników z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook*:

![](https://i.imgur.com/F8FIZvi.png)

Tezę tą można poprzeć wprowadzając następującą modyfikację do `mkholes`:

```c=
#define NBLKS 8192 * 4 /* number of blocks written to a file */
```

Wtedy w pliku znajdzie się 539 niepustych bloków, a zaalokowane zostanie 541, podobnie jeśli liczbę to zmniejszy się tak żeby niepustych było 5, to nie zostanie zaalokowany żaden dodatkowy blok, co wskazuje na powiązanie liczby dodatkowych bloków z liczbą bloków z danymi.
# Systemy Operacyjne -- Lista 2
###### tags: `SO`

## Zadanie 1

![](https://i.imgur.com/dLLP3eM.png)

W systemie linux możemy wyróżnić następujące stany procesu:

* Running, w którym możemy wyróżnić dwa podstany
	* Executing -- instrukcje procesu są obecnie wykonywane przez procesor.
	* Ready -- proces jest gotowy do przejścia w stan executing (ma wszystkie potrzebne dane i zasoby), ale z jakiegoś powodu nie został mu obecnie przydzielony czas procesora przez scheduler (czeka on w kolejce na bycie wykonywanym).
* Zombie -- proces zakończył się, ale np. nie został pogrzebany przez rodzica, więc wciąż musi istnieć wpis w tabeli procesów odpowiadający temu taskowi.
* Blocked, w którym również możemy wyróżnić dwa podstany
	* Intrruptible -- stan, w którym proces oczekuje na zdarzenie, takie jak zakończenie operacji wejścia/wyjścia, dostęp do zasobów, lub sygnał z innego procesu.
	* Uninterruptible -- również czeka na zdarzenie, jednak zazwyczaj jest ono powiązane bezpośrednio ze sprzętem. W tym stanie proces nie obsłuży sygnału aż do momentu, w którym zostanie wybudzony przez zdarzenie na które czeka.
* Stopped -- proces, który został zatrzymany sygnałem i może wznowić działanie tylko w przypadku otrzymania kolejnego sygnału od innego procesu. Przykładem tych przejść może być naciśnięcie `ctrl+Z`, które wysyła sygnał `SIGTSTP`, a następnie wznowienie programu wysłaniem sygnału `SIGCONT`.

### Akcje wyzwalające zmiany stanów

* Scheduling, czyli zmiany obecnie wykonywanego procesu
* Różnego rodzaju sygnały
* Zdarzenia na które procesy oczekują snem nieprzerywalnym
* Zakończenie programu

### Które z tych przejść może być wywołane przez:

#### Jądro systemu

Ready -> Executing i Executing -> Ready przy zmianach kontekstu. 
Executing  -> Zombie przy zakończeniu działania programu wewnątrz procesu.
Executing -> Interruptible, Interruptible -> Ready przy obsłudze przerywania czytającego z wejścia/wyjścia (np. read).

#### Kod sterowników

Executing -> Uninterruptible i Uninterruptble -> Ready np. przy pobieraniu danych z dysku twardego.

#### Proces użytkownika

Executing -> Stopped i Stopped -> Ready przy wysyłaniu sygnałów `SIGTSTP` i `SIGCONT`. Executing -> Interruptible i Interruptible -> Ready np. kiedy proces zaczyna pauzować w oczekiwaniu na sygnał, lub wysyła innemu procesowi sygnał w celu wybudzenia go z tej pauzy.

### Czy można zablokować/zignorować SIGKILL/SIGSEGV

Zablokowanie i zignorowanie sygnału `SIGSEGV` jest możliwe (chociaż to drugie nie jest dobrym pomysłem), natomiast zgodnie z manualem `signal(2)` i `sigprocmask(2)` sygnał `SIGKILL` jest niemożliwy do zablokowania, zignorowania, ani złapania.

## Zadanie 2

### Różnice pomiędzy tworzeniem procesu w Linuxie, a w Windowsie

Wiemy, że procesy w systemie Linux tworzone są przez wywołanie systemowe `fork`, które tworzy kopię wołającego procesu. Żeby zrozumieć różnicę między tymi dwoma systemami należy przybliżyć sposób w jaki procesy są tworzone w systemie Windows:

* Najpierw musi nastąpić wywołanie procedury z API systemu Windows -- Win32, która nazywa się CreateProcess
* Procedura ta najpierw przetłumaczy podaną nazwę programu na ścieżkę NT, a następnie zawoła procedurę z native API -- NTCreateUserProcess. Będzie ona wykonywana w trybie nadzorcy.
* Procedura ta tworzy obraz procesu, który następnie zostaje użyty do zmapowania programu na przestrzeń adresów wirtualnych oraz inicjalizowane są: struktura jądra reprezentująca proces, katalogi stron, deskryptory adresów wirtualnych, struktury wybierające strony do odrzucenia gdy kończy się fizyczna pamięć, PEB (process environment block)
* Alokowana jest pamięć wewnątrz nowo utworzonego procesu, w której umieszczane są parametry polecenia, zmienne środowiskowe itd.
* Nadany jest identyfikator procesu
* Utworzony jest obiekt wątku
* Proces zostaje dodany do globalnej listy procesów, a jedyny, na razie zawieszony, wątek otrzymuje ID
* Procedura NTCreateUserProcess kończy działanie, czyli przestajemy operować w trybie nadzorcy
* Proces jest rejestrowany w podsystemie Win32
* Wątek procesu jest wznawiany, a procedura CreateProcess kończy działanie i zwraca ID

Najważniejszą różnicą w stosunku do Linuxa jest to, że utworzenie procesu jest jednoznaczne z załadowaniem do niego nowego programu, więc nowy proces nie ma kopii przestrzeni adresowej swojego rodzica. Można też zauważyć że w systemie Windows istnieje rozróżnienie między procesem, a wątkiem.

### Akcje podejmowane w trakcie fork i execve

#### fork

* Wywoływana jest *pułapka* i kontrola przenosi się do jądra systemu
* Inicjalizowana jest struktura jądra reprezentująca zadanie oraz struktury dodatkowe, takie jak jak *thread_info*, która przechowuje m.in. wskaźnik na deskryptor procesu (PCB).
* Deskryptor procesu jest w większości kopiowany z rodzica
* Procesowi nadany jest niużywany przez inny proces ani grupę identyfikator.
* Proces jest wstawiany do tablicy procesów oraz dołączany do listy wiązenej procesów w odpowiednie miejsce.
* Proces otrzymuje kopię przestrzeni adresowej rodzica w formie *copy on write*
* W tym momencie jest już gotowy do uruchomienia

#### execve

* Wyszukiwany jest plik wykonywalny do którego odnosi się pierwszy argument
* Argumenty polecenia i zmienne środowiskowe są kopiowane do wnętrza jądra systemu, następnie zwalniany są jego tablice stron.
* Pusta teraz przestrzeń jest teraz na nowo wypełniana. Plik wykonywalny zostanie zmapowany na przestrzeń, ale nie będzie załadowany w całości do pamięci fizycznej, przez co zostanie leniwie wciągnięty do pamięci page-faultami.
* Argumenty polecenia oraz zmienne środowiskowe zostają wg konwencji skopiowane na stos nowego programu
* Sygnały i rejestry są zerowane
* Nowy program może zacząć działać

### Dlaczego wywołanie spawn to w ogólności zły pomysł?

Rozdzielenie tych dwóch wywołań otwiera przed nami wiele możliwości kontrolowania pierwszych instrukcji jakie wykonywane są w dziecku. Jest to szczególnie przydatne w implementacji shell'a, kiedy łączymy dwa procesy za pomocą potoku.

Potok to plik, który działa jak kolejka FIFO i służy do łączenia wyjścia jednego procesu z wejściem drugiego. Bardzo prosto to zaimplementować mając osobne procedury `fork` i `exec`

```
pid = fork
	...
	if(pid==0)
		zamykamy deskryptor pliku stdout/stdin
		otwieramy interesujący nas plik(i) potoku
		wywołujemy exec*
```

Ponieważ wywołania `exec` zachowują deskryptory pliku, oraz na nowy deskryptor zawsze wybierany jest najniższy wolny, to w miejscu standardowego wejścia/wyjścia w wywołanym programie znajdzie się ten plik.

## Zadanie 3

### Zasoby procesu dziedziczone przez potomka

* Otwarte deskryptory plików
* Kopię przestrzeni adresowej rodzica (copy-on-write), w tym
	* kopię stosu i sterty
	* współdzielone segmenty pamięci
	* mapowania pamięci
* Liczne własności rodzica:
	* RUID, RGID, EUID, EGID
	* Grupy dodatkowe, do których należy użytkownik
	* ID grupy procesów
	* Terminal kontrolujący
	* Flagi set-user-ID i set-group ID
	* Katalog bieżący procesu
	* Katalog root
	* Maskę trybów tworzenia plików
	* Maskę blokowanych sygnałów razem z handlerami **(maska sygnałów oczekujących zostaje wyzerowana)**
	* Flagę close-on-exec dla otwartych descryptorów plików
	* Środowisko
	* Ograniczenia dotyczące zasobów

### Czemu przed wywołaniem fork należy opróżnić bufor?

Rozważmy poniższy program

```c=
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<sys/wait.h>

int main(){

    printf("Hello\n");

    int pid = fork();
    if(pid == 0){
        printf("This is child\n");
    }
    else{
        waitpid(pid, NULL, 0);
        printf("This is parent\n");
    }

    return 0;
}
```
Po uruchomieniu w trybie interaktywnm nie zauważymy nic dziwnego, jednak po przekierowaniu wyjścia do pliku, okaże się że linia "Hello" zostanie wydrukowana dwa razy.

```
cezary@cezary-MSI$ ./a.out
Hello
This is child
This is parent
cezary@cezary-MSI$ ./a.out > test.txt
cezary@cezary-MSI$ cat test.txt
Hello
This is child
Hello
This is parent
cezary@cezary-MSI$  
```

Dzieje się tak ponieważ w trybie interaktywnym wyjście w stdio jest buforowane aż do napotkania znaku nowej lini, a w innym przypadku w pełni buforowane, dlatego zarówno w dziecku jak i rodzicu string "Hello" wciąż będzie znajdował się w buforze i zostanie wypisany przy zakończeniu programu.

### Zasoby przekazywane do nowego programu

* Deskryptory plików które nie były oznaczone flagą close-on-exec
* Identyfikator procesu i identyfikator rodzica procesu oraz identyfikator grupy procesów
* RUID, RGID
* Dodatkowe grupy użytkownika
* Identyfikator sesji
* Terminal kontrolujący
* Pozostały czas do kolejnego sygnału SIGALRM
* Biężący katalog
* Katalog root
* Maskę trybów tworzenia plików
* Blokady na plikach
* Maskę blokowanych sygnałów
* Maskę sygnałów oczekujących
* Ograniczenia dotyczące zasobów
* Wartość nice (priorytet (tak z grubsza))
* Wartości tms_utime, tms_stime, tms_cutime, tms_cstime

### Co jądro robi w trakcie wywołania execve z handlerami

Zgodnie z manualem system calla execve:

![](https://i.imgur.com/teHc4oC.png)

Zarejestrowane handlery łapanych sygnałów zostają zastąpione domyślnymi.

## Zadanie 4

![](https://i.imgur.com/7CRDbc4.png)

### kill

Polecenie `kill` służy do wysyłania sygnałów do procesu. Niekoniecznie musi to być sygnał `KILL`. Podajemy do niego `pid` procesu.

`kill $(pidof xeyes)`

Możemy podejrzeć jaki sygnał został wysłany do procesu dzięki temu, że zostaje on zakodowany w kodzie wyjścia programu. `kill` z flagą `-l` po podaniu kodu jako argumentu wyświetla nazwę sygnału.

`kill -l $(echo $?)`

![](https://i.imgur.com/wOUW242.png)

Jak widać proces został zakończony sygnałem TERM.

### pkill

Działa podobnie jak `kill`, ale pozwala na wysłanie sygnału do procesu wyszukując go po jego nazwie za pomocą wyrażenia regularnego.

`pkill xeyes`

`kill -l $(echo $?)`

![](https://i.imgur.com/ntjQGbM.png)
 
### xkill

Polecenie podobne do powyższych. Różnica polega na tym, że to polecenie zmusza do zakończenia proces powiązany z wybranym za pomocą specjalnego kursora okna X serwera.

`xkill`

![](https://i.imgur.com/5JrSHUY.png)

Tym razem proces został zakończony sygnałem HUP

### SIGSTP i SIGCONT

Zatrzymujemy proces sygnałem `SIGSTP` wysłanym w wyniku wciśnięcia kombinacji klawiszy `ctrl + z`.

![](https://i.imgur.com/Hg5NiHI.png)

Możemy teraz wznowić wykonywanie ręcznie wysyłając sygnał `SIGCONT`

![](https://i.imgur.com/4qFLY5P.png)

Lub wznowić i od razu przenieśc na pierwszy plan poleceniem `fg`

![](https://i.imgur.com/pngN6pl.png)

### SIGUSR1, SIGUSR2, SIGHUP, SIGINT

`SIGUSR1`, `SIGUSR2` o numerach odpowiednio 10 i 12 to sygnały, których znaczenie definiuje użytkownik, `SIGHUP` o numerze 1 sygnalizuje śmierć kontrolującego procesu, lub terminala, `SIGINT` o numerze 2 -- przerwanie wywołane przez klawiaturę.

Maskę oczekujących sygnałów możemy obejrzeć pod polem `ShdPnd` w pliku `proc/{pid}/status`.

Wartość początkowa:

![](https://i.imgur.com/WwMXDcT.png)

Po wysłaniu SIGUSR1 `kill -USR1 $(pidof xeyes)`:

![](https://i.imgur.com/2xP653u.png)

Zapalił się dziesiąty bit (bo numer sygnału to 10). Kolejne sygnały będą powodowały pododny efekt.

`kill -USR2 $(pidof xeyes)`

![](https://i.imgur.com/hWbrZ0V.png)


`kill -HUP $(pidof xeyes)`

![](https://i.imgur.com/VMNG9nu.png)


`kill -INT $(pidof xeyes)`

![](https://i.imgur.com/SF3L3om.png)

### Co opisują pozostałe pola pliku status?

Jest to kilka innych masek bitowych:
`SigPnd` - oczekujące sygnały dla wątku
`ShdPnd` - oczekujące sygnały dla całego procesu
`SigBlk` - sygnały blokowane
`SigIgn` - sygnały ingnorowane
`SigCgt` - sygnały łapane

### Który sygnał dotrze jako pierwszy?

Nie wiadomo..., a przynajmniej nie można założyć że istnieje jakaś kolejność. Z manuala:

```
Queueing and delivery semantics for standard signals
       If multiple standard signals are pending for a process, the order
       in which the signals are delivered is unspecified.
```

W praktyce, pierwszy dotarł `HUP`:

![](https://i.imgur.com/lduu6Iy.png)

Czyli proces, który (nie przypadkiem) ma najniższy numer.

## Zadanie 5

![](https://i.imgur.com/GPIZvp6.png)

Przyjrzyjmy się po kolei częścią funkcji `main`

```c=
int
main(void)
{
	int sig;
	size_t i;

	if (getpid() != 1)
		return 1;
```

Na początku proces sprawdza czy jest procesem o id równym 1 (czyli czy na pewno jest initem).

```c=
	chdir("/");
	sigfillset(&set);
	sigprocmask(SIG_BLOCK, &set, NULL);
```

Następnie zmienia katalog do `/` po czym wypełnia zbiór sygnałów `set` każdą możliwą flagą, a następnie ustawia zbiór blokowanych sygnałów na `set` za pomocą procedury `sigprocmask`. Procedura `sigprocmask` ma sygnaturę `int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);` i służy do blokowania, lub odblokowywania sygnałów w zależności od pierwszego argumentu (`SIG_BLOCK`/`SIG_UNBLOCK`). W pierwszym przypadku do obecnego zbioru zostaje dosumowany `set`, w drugim zostaje odjęty. W obu przypadkach poprzedni stan trafia do `oldset`. Można też po prostu nadpisać obecny zbiór na `set` podając w pierwszym argumencie `SIG_SETMASK`.


```c=
	spawn(rcinitcmd);
```

Procedura `spawn` forkuje, a następnie w dziecku wykonuje `execve` uruchamiając podane polecenie. W tym przypadku będzie to polecenie uruchamiające kolejne komponenty systemu.

```c=
	while (1) {
		alarm(TIMEO);
		sigwait(&set, &sig);
```
Rozpoczyna się główna pętla programu. Zaplanowywane jest dostarczenie sygnału `SIGALARM` za `TIMEO` sekund, a następnie wykonanie programu jest zawieszane aż którykolwiek sygnał się pojawi. Osiągane to jest poprzez procedurę `int sigwait(const sigset_t *set, int *sig);`, która oczekuje na dowolny sygnał zdefiniowany w `set`, a następnie zwraca przez `sig` numer tego który się pojawił.

```c=
		for (i = 0; i < LEN(sigmap); i++) {
			if (sigmap[i].sig == sig) {
				sigmap[i].handler();
				break;
			}
		}
	}
	/* not reachable */
	return 0;
}
```

Kiedy sygnał przybędzie iterujemy się po tablicy `sigmap`, która przechowuje struktury przyporządkowujące sygnał do handlera tego sygnału. Jeśli napotkamy numer sygnału, który przybył, wywołujemy jego handler.

### Jakie akcje są wykonywane dla sygnałów w sigmap?

```c=
static struct {
	int sig;
	void (*handler)(void);
} sigmap[] = {
	{ SIGUSR1, sigpoweroff },
	{ SIGCHLD, sigreap     },
	{ SIGALRM, sigreap     },
	{ SIGINT,  sigreboot   },
};
```

Po napotkaniu `SIGUSR1` wyłączany jest system poleceniem zdefiniowanym przez `rcpoweroffcmd`:

```c=
static void
sigpoweroff(void)
{
	spawn(rcpoweroffcmd);
}
```

Po napotkaniu `SIGCHLD` lub `SIGALRM` grzebane są dzieci:

```c=
static void
sigreap(void)
{
	while (waitpid(-1, NULL, WNOHANG) > 0)
		;
	alarm(TIMEO);
}
```

Po napotkaniu `SIGINT` wykonywane jest polecenie odpowiedzialne za restart systemu:

```c=
static void
sigreboot(void)
{
	spawn(rcrebootcmd);
}
```

### W jaki sposób proces grzebie swoje dzieci?

Jak widzieliśmy wyżej proces czeka na jakiekolwiek dziecko które zmieniło stan i efektywnie je grzebie. Jeśli dzieci istnieją, ale żadne z nich nie zmieniło stanu, to dzięki wyspecyfikowaniu opcji `WNOHANG` zwrócone zostanie 0 i nie będziemy zawieszać działania programu w oczekiwaniu na zakończenie działania wszystkich dzieci (co mogłoby mieć smutne skutki np. w postaci braku możliwości wyłączenia systemu).

## Zadanie 6

![](https://i.imgur.com/5mYLsv9.png)

Procedura `prctl` służy do wykonywania różnego rodzaju operacji na procesie. Z pierwszym argumentem `PR_SET_CHILD_SUBREAPER` i drugim `1` nada obecnemu procesowi rolę żniwiarza, czyli przejmie on rolę `init`'a dla swoich potomków. To do niego (lub do innego najbliższego im żniwiarza) będą przypinane osierocone procesy, które były jego potomkami.

### Dziecko

Tworzy wnuka i kończy działanie
```c
static void child(void) {
  pid_t pid;

  /* TODO: Spawn a child! */
  Setpgid(0,0);
  pid = spawn(grandchild);

  printf("(%d) Grandchild (%d) spawned!\n", getpid(), pid);
}
```

### Wnuk

Zatrzymuje wykonanie i oczekuje na sygnał

```c
static void grandchild(void) {
  printf("(%d) Waiting for signal!\n", getpid());
  /* TODO: Something is missing here! */
  pause();

  printf("(%d) Got the signal!\n", getpid());
}
```

### PS

Tworzy forka, po czym w dziecku podmienia przestrzeń adresową na `ps` z podanymi argumentami

```c
static void ps(void) {
  /* TODO: Something is missing here! */
  pid_t pid = Fork();
  if(pid == 0) {
    char* pscommand[] = {"/bin/ps", "-o", "pid,ppid,pgrp,stat,cmd", NULL};
    execve(pscommand[0], pscommand, NULL);
    perror("execve");
    exit(1);
  }
  Waitpid(pid,NULL,0);
}
```

### Main


```c
int main(void) {
  /* TODO: Make yourself a reaper. */
#ifdef LINUX
  Prctl(PR_SET_CHILD_SUBREAPER, 1);
#endif
  printf("(%d) I'm a reaper now!\n", getpid());

  pid_t pid, pgrp;
  int status;

  /* TODO: Start child and grandchild, then kill child!
   * Remember that you need to kill all subprocesses before quit. */

  pid = spawn(child);
  pgrp = pid;
  Waitpid(pid, NULL, 0);
  ps();

  Kill(-pgrp, SIGINT);
  Waitpid(-1, &status, 0);
  printf("(%d) Grandchild exit status: %d\n", getpid(), status);

  return EXIT_SUCCESS;
}
```

## Zadanie 8

Lista procedur wielobieżnych z podręcznika:

![](https://i.imgur.com/F2ibDMf.png)
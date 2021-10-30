# Systemy Operacyjne -- Lista 3
###### tags: `SO`

## Zadanie 1

### Kto przygarnie osierocony proces?

![](https://i.imgur.com/2C0rBYe.png)

### Co się dzieje z sesją która utraci terminal sterujący?

Przeprowadzimy eksperyment:

![](https://i.imgur.com/nG5080o.png)

Wynik polecenia `strace -e trace=signal -p $(pidof bash)`

![](https://i.imgur.com/iAl1Sm1.png)

Okazuje się, że w reakcji na utracenie terminala sterującego proces otrzymał sygnał `SIGHUP`, czyli sygnał który jest wysyłany do precesu sterującego kiedy jego terminal kontrolujący zostanie odłączony. Domyślną reakcją na ten sygnał jest zakończenie procesu.

## Zadanie 2

### Jak zachowuje się sterownik terminala działający w trybie kanonicznym?

#### Wczytywanie

* Jednostką na wejściu jest zakończona znakiem EOL, NL, albo EOF linia
* Operacja czytania nie zakończy się dopóki nie zostanie wprowadzony znak kończący linię, lub nie zostanie odebrany sygnał.
* Można wczytać mniej bajtów niż jest w lini, pozostała część lini nie zostanie utracona (można ją wczytać kolejnym readem).
* W jednej lini nie może znaleźć się więcej bajtów niż zdefinowane jest w MAX_CANNON
* Sterownik terminala po napotkaniu specjalnych znaków może przeprowadzać operacje na danych już znajdujących się w kolejce wejściowej, ale jeszcze nie zakończonych znakiem końca lini. Są to np. znaki `kill` i `erase`, o których więcej powiedziane będzie w następnym zadaniu. Znaki te nie są umieszczane w kolejce.

#### Wypiswanie

* Kiedy zlecone zostaje wypisanie bajtów, przetwarzanie ich odbywa się według flag ustawionych w polu `c_oflag`, których znaczenie jest następujące
![](https://i.imgur.com/wzjUj96.png)
* Jeśli sterownik implementuje buforowanie, znaki po operacji wypisania zostaną zlecone do wypisania, ale niekoniecznie wypisane.

### W jaki sposób przetwarzane są znaki w kolejkach?

![](https://i.imgur.com/JkMnhdP.png)

Maksymalna długość kolejki jest definiowana przez `MAX_INPUT`.

### Jak konfigurację terminala powinien zmienić program przy wpisywaniu hasła?

Dobrym pomysłem byłoby wyłączenie echa, co można osiągnąć poprzez edycję pola `c_lflag` i bitu `ECHO`.

### Dlaczego edytory (np. Vi) nie działają w trybie kanonicznym?

Edytory te konfigurują swój tryb, nadpisując domyślne interpretacje niektórych symboli specjalnych, np. jeśli chcielibyśmy żeby można było kopiować skrótem `ctrl + c`, to raczej nie chcemy żeby sterownik terminala wysłał w tym przypadku sygnał `SIGINT` do grupy pierwszoplanowej.

## Zadanie 3

### Wydruk polecenia stty -a

![](https://i.imgur.com/ddMieu7.png)

### Znaki specjalne

#### Do zarządzania zadaniami

* `intr` -- służy do wysyłania sygnału `SIGINT` do grupy pierwszoplanowej.
* `quit` -- do wysyłania `SIGQUIT`
* `swtch` -- zmienia warstwę powłoki (cokolwiek to znaczy)
* `susp` -- do wysyłanie `SIGSTOP`
* `discard` -- włącza ignorowanie wyjścia (wysyła je do /dev/null, ale wyglada na to że na linuxie jeszcze nie działa (i chyba nigdy nie będzie?))

![](https://i.imgur.com/jQUHQdO.png)

![](https://i.imgur.com/asUahya.png)


#### Do edycji wiersza

* `erase` -- usuwa ostatni wprowadzony znak
* `kill` -- usuwa obecną wprowadzoną linię
* `eof` -- wysyła znak końca pliku
* `eol` -- kończy wprowadzoną linię
* `eol2` -- alternatywa dla `eol`
* `stop` -- tymczasowo zatrzymuje wypisywanie wyjścia
* `start` -- wznawia wypisywanie
* `rprnt` -- wypisuje ponownie obecną linię
* `werase` -- usuwa ostatnie wpisane słowo
* `lnext` -- kolejny znak nie będzie traktowany jako specjalny

### Reagowanie na zmianę okna

Istnieje specjalny sygnał, który zostaje wysyłany do procesu przy zmianie rozmiaru terminala, jest to `SIGWINCH` i jest domyślnie ignorowany. Nowy rozmiar można wczytać za pomocą biblioteki `ioctl` oraz requestu `TIOCGWINSZ`. Poniżej przykładowy program przechwytujący zdarzenie zmiany okna i wczytujący nowe rozmiary

```c=
#include "csapp.h"
#include "sys/ioctl.h"

static void signal_handler(int signo) {
    struct winsize size;

    ioctl(0,TIOCGWINSZ, &size);
    safe_printf("Window resized: %d x %d\n", size.ws_row, size.ws_col);
}

int main(void) {
    Signal(SIGWINCH,signal_handler);
    
    while (1) continue;
    
    return 0;
}
```

## Zadanie 5

### Cat abuse
### Jaki sygnał wysyła bg?
Łatwo się przekonać strace'em.

![](https://i.imgur.com/sTTb5l8.png)

Do zatrzymania procesu i kontynuowania go w tle oczywiście użyte są sygnały `SIGSTP` i `SIGCONT`. 

### Dlaczego wyjście finda się zatrzymuje i wznawia?

`CTRL+S` i `CTRL+Q` to zgodnie z wydrukiem `stty -a` znaki specjalne terminala -- odpowiednio `stop` i `start`. Zatrzymują/wznawiają one wypisywanie wyjścia procesu. Nie zauważy on zmiany, gdyż żaden sygnał nie jest wysyłany, terminal po prostu zacznie buforować jego wyjście w czasie wstrzymania, a następnie wypisze po wznowieniu.

### Dlaczego cat - & od razu się zatrzymał?

![](https://i.imgur.com/wlcrBzx.png)

Za pomocą `strace'a` możemy podejrzeć, że zatrzymał się w reakcji na sygnał `SIGTTIN`. Dzieje się tak ponieważ został uruchomiony w tle, a następnie chciał czytać z wejścia standardowego. Nie może tego zrobić, gdyż tę akcję może wykonać tylko grupa pierwszoplanowa, więc powłoka wyśle do niego ten sygnał żeby zasygnalizować tę sytuację. Domyślną reakcją na niego jest zatrzymanie.
    
### Z tostopem i bez

`tostop` to opcja sterownika terminala, która zmienia domyślną reakcję na próbę zapisu przez procesy działające w tle.

![](https://i.imgur.com/VzykVba.png)

Włączona sprawia, że każda próba takiego zapisu skończy się zatrzymaniem procesu. Co widoczne jest na przykładzie:

![](https://i.imgur.com/EXVO0PO.png)


## Zadanie 6

### Setjump

Pod `%rdi` znajdzie się wskaźnik na argument, który jest strukturą typu Jmpbuf:

```c=
/* Setjmp & longjmp implementation without sigprocmask */
typedef struct {
  long rbx;
  long rbp;
  long r12;
  long r13;
  long r14;
  long r15;
  void *rsp;
  void *rip;
} Jmpbuf[1];
```

Jak widać jest tutaj miejsce na część rejestrów, które są z rodziny callee-saved (`%rbp`, `%rbx` i `%r12`-`%r15`), oraz na wskaźnik stosu `rsp` i wskaźnik na obecną instrukcję `rip`. nie ma potrzeby zapamiętywania rejestrów z rodziny caller-saved, ponieważ jeśli wołąjąca procedura przetrzymuje tam istotne dane, to zgodnie z konwencją powinna je zapisać na stosie przed zawołaniem procedury `Setjmp`.

Przebieg procedury `Setjmp` jest następujący

```
Setjmp:
	movq    (%rsp),%r11 // odkładamy adres powrotu do r11 (posłuży nam jako rip w zapisanym kontekście)
	movq    %rbx,(_JB_RBX * 8)(%rdi) // ^
 	movq    %rbp,(_JB_RBP * 8)(%rdi) // |
	movq    %r12,(_JB_R12 * 8)(%rdi) // |
	movq    %r13,(_JB_R13 * 8)(%rdi) // |
	movq    %r14,(_JB_R14 * 8)(%rdi) // |
	movq    %r15,(_JB_R15 * 8)(%rdi) // |
	movq    %rsp,(_JB_RSP * 8)(%rdi) // |
	movq    %r11,(_JB_RIP * 8)(%rdi) // v zapisujemy kontekst
	xorl	%eax,%eax // zerujemy raxa
	ret     // i to zero zwracamy
```

### Longjump

Tym razem w `%rdi` dotrze wskaźnik na `Jmpbuf`, a w `%rsi` wartość, którą chcemy zwrócić z `Setjump`. Musi być ona różna od zera, gdyż zero jest zarezerwowane dla pierwszego wywołania `Setjump`. W przypadku pomyłki, zgodnie z dokumentacją zwracamy `1`.

```
If the programmer mistakenly passes the value 0 in val, the "fake" return will 
instead return 1.
```


```
Longjmp:
	movq    (_JB_RBX * 8)(%rdi),%rbx // ^
	movq    (_JB_RBP * 8)(%rdi),%rbp // |
	movq    (_JB_R12 * 8)(%rdi),%r12 // |
	movq    (_JB_R13 * 8)(%rdi),%r13 // |
	movq    (_JB_R14 * 8)(%rdi),%r14 // |
	movq    (_JB_R15 * 8)(%rdi),%r15 // |
	movq    (_JB_RSP * 8)(%rdi),%rsp // |
	movq    (_JB_RIP * 8)(%rdi),%r11 // v odtworzenie kontekstu
	movl	%esi,%eax // przenosimy wartość drugiego argumentu do rejestru przez który zwracamy
	testl	%eax,%eax // sprawdzamy czy jest zeren
	jnz	1f // jeśli nie jest to skocz
	incl	%eax // jeśli jest to zwiększ o 1
1:	movq	%r11,(%rsp) // Nadpisz adres powrotu adresem ze struktury
	ret
```

#### Dlaczego na koniec coś umieszczane jest na stosie?

Instrukcję `ret` można zareprezentować za pomocą ciągu innych instrukcji, mianowicie pobrania ostatniej wartości ze stosu i skoczenia pod nią. Zatem umieszczając w tym miejscu zachowany w strukturze wskaźnik na instrukcję, po wykonaniu `ret` znajdziemy się w kodzie zaraz po wywołaniu `setjmp` (ponieważ ten `rip` był adresem powrotu z niej).

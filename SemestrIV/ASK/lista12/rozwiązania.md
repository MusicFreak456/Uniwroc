# ASK -- Lista 12
###### tags: `ASK`

## Zadanie 1

### Dlaczego procedura nie może zostać zoptymalizowana?

Rozważamy procedurę `swap`, której kod to

```c=
void swap(long *xp, long *yp) {
    *xp = *xp + *yp; /* x+y */
    *yp = *xp - *yp; /* x+y-y = x */
    *xp = *xp - *yp; /* x+y-x = y */
}
```

nie jest ona tłumaczona przez kompilator optymalizującej do lepszej postaci `swap2`

```c=
void swap2(long *xp, long *yp) {
    long x = *xp, y = *yp;
    x = x + y, y = x - y, x = x - y;
    *xp = x, *yp = y;
}
```

Dzieje się tak przez zjawisko nazywane **aliasingiem pamięci**, polega ono na tym, że dwa symbole w programie mogą odnosić się do tych samych miejsc w pamięci. Np. w naszej procedurze `swap` wskaźnik `xp` oraz `yp` może wskazywać na tą samą komórkę, przez co zapis pod `xp` może zmienić wartość pod `yp`. Dlatego zoptamylizowanie procedury do postaci`swap2` mogłoby zmienić zachowanie programu.

```c=
long a = 2;
long b = 2;

swap(&a, &a);
swap2(&b, &b);

// a = 0
// b = 2
```

### Jak pomóc kompilatorowi?

Język C pozwala nam zapewnić kompilator że wskaźniki nigdy nie będą wskazywać na to samo miejsce w pamięci poprzez użycie słowa kluczowego `restrict`. Jest to rodzaj "umowy" między programistą i kompilatorem, nie jest to sprawdzane na żadnym etapie kompilacji ani po uruchomieniu.

```c=
void faster_swap(long *restrict xp, long *restrict yp) {
    *xp = *xp + *yp; /* x+y */
    *yp = *xp - *yp; /* x+y-y = x */
    *xp = *xp - *yp; /* x+y-x = y */
}
```

#### Kod wynikowy swap

```=
swap:
        movq    (%rsi), %rax
        addq    (%rdi), %rax
        movq    %rax, (%rdi)
        subq    (%rsi), %rax
        movq    %rax, (%rsi)
        subq    %rax, (%rdi)
        ret
```

#### Kod wynikowy faster_swap

```=
faster_swap:
        movq    (%rsi), %rax
        movq    (%rdi), %rdx
        movq    %rdx, (%rsi)
        movq    %rax, (%rdi)
        ret
```

Jak widać nie jest to do końca procedura `swap2`, gdyż kompilator rozpoznał że to co robimy to zamiana miejscami wartości zmiennych i zoptymalizował ją do postaci

```c=
void faster_swap(long *restrict xp, long *restrict yp) {
	long xp_t = *xp;
	long yp_t = *yp;
	*xp = yp_t;
	*yp = xp_t;
}
```

## Zadanie 2

![](https://i.imgur.com/NdiACe7.png)


### Ile razy zostanie wywołana funkcja i dlaczego?

Wystarczy rzucić okiem na kod wynikowy procedury `my_index`

```=
my_index:
        movq    %rdi, %rcx
        xorl    %edx, %edx
        jmp     .L8
.L10:
        cmpb    %sil, (%rcx,%rdx)
        leaq    (%rcx,%rdx), %rax
        je      .L7
        addq    $1, %rdx
.L8:
        movq    %rcx, %rdi
        call    my_strlen
        cmpq    %rdx, %rax
        ja      .L10
        xorl    %eax, %eax
.L7:
        rep ret
```

Jak widać, funkcja jest wołana w momencie sprawdzania warunku (linie 10-15). Czyli funkcja będzie wołana przy każdym obrocie pętli, mimo że wartości w stringu `s` nie są modyfikowane.

#### Czy usunięcie inline pomoże?

Można to sprawdzić sprawdzając kod wynikowy bez tego atrybutu

```=
my_index:
        movzbl  (%rdi), %ecx
        xorl    %edx, %edx
        xorl    %eax, %eax
        testb   %cl, %cl
        je      .L7
.L10:
        addq    $1, %rax
        cmpb    $0, (%rdi,%rax)
        jne     .L10
        cmpq    %rax, %rdx
        jnb     .L14
        cmpb    %sil, (%rdi,%rdx)
        leaq    (%rdi,%rdx), %rax
        je      .L7
        addq    $1, %rdx
        xorl    %eax, %eax
        testb   %cl, %cl
        jne     .L10
.L7:
        rep ret
.L14:
        xorl    %eax, %eax
        ret
```

Sprawdzanie warunku zachodzi teraz w liniach 8-12 i widzimy że wciąż wykonywane jest w nim ciało funkcji `my_strlen`.

#### Dlaczego tak się dzieje?

Kompilator mimo że posiada informację, że string nie będzie modyfikowany, to nie może poczynić założenia, że funkcja `my_strlen` nie ma efektów ubocznych, czyli że jest czysta (nie modyfikuje stanu programu w inny sposób niż przez zwracaną wartość). Możemy go o tym zapewnić dodając atrybut `pure`.

```c=
__attribute__((noinline))
__attribute__((pure))
size_t my_strlen(const char *s) {
    size_t i = 0;
    while (*s++) 
        i++;
    return i;
}
```

Teraz po ponownej kompilacji, w końcu otrzymujemy kod, w którym funkcja `my_strlen` jest wywoływana tylko raz, na początku.

```=
my_index:
        call    my_strlen
        leaq    (%rdi,%rax), %rdx
        jmp     .L8
.L10:
        movq    %rdi, %rax
        addq    $1, %rdi
        cmpb    %sil, -1(%rdi)
        je      .L7
.L8:
        cmpq    %rdx, %rdi
        jne     .L10
        xorl    %eax, %eax
.L7:
        rep ret
```

## Zadanie 3

![](https://i.imgur.com/InnWo83.png)

### Kod wynikowy 

```=
foobar:
        testq   %rsi, %rsi            # jeśli n=0 skaczemy na koniec
        je      .L1
        subq    %rcx, %rdx            # x = y - z
        leaq    (%rdi,%rsi,8), %rax   # &a[n]
        imulq   %rdx, %rdx            # x * x
.L3:
        movq    %rdx, (%rdi)          # a[i] = x * x
        addq    $8, %rdi              # i ++
        addq    $7, %rdx              # x * x + 7 * i
        cmpq    %rax, %rdi            # jeśli &a[i] == &a[n] kończymy
        jne     .L3
.L1:
        ret
```

#### Wersja w C

```c=
void foobar(long a[], size_t n, long y, long z) {
    long x = y - z;
    long end = a + n * 8;
    long val = x * x;
    
    for(long dest = a; dest < end; dest += 8){
        *(dest) = val;
        val += 7;
    }
}
```

### Niezmienniki pętli

Niezmiennik pętli (ang. *loop-invariant code*) to wyrażenia, które mogą zostać wyniesione poza ciało pętli bez wpływania na jej działanie. W naszym kodzie są dwa takie wyrażenia 

* `x * x`
* `x = y - z`

i w zoptymalizowanym kodzie zostały wyniesione poza pętlę.

### Zmienne indukcyjne 

Czyli zmienne które są pomniejszane lub zwiększane o stałą w każdej iteracji, lub zależą liniowo od innej zmiennej indukcyjnej. W powyższym kodzie występuje dwie takie zmienne: `i` i `j`.

### Osłabione wyrażenia

Osłabienie (ang. *strength reduction*) to zamiana wyrażenień, które są cieższe do obliczenia przez procesor, na lżejsze. W optymalizacji powyższego kodu sytuacja taka miała miejsce gdy wyrażenie `j = 7 * i` zostało (w praktyce) zamienione na  `j += 7` w każdej pętli, gdyż kompilator zauważył że `j` to zmienna indukcyjna zależna od innej zmiennej indukcyjnej `i`, która jest zwiększana o 1 przy każdym obrocie, stąd `j` będzie zwiększane o 7.

## Zadanie 4

![](https://i.imgur.com/QkvjSeO.png)

### Kod wynikowy procedury neigh
```=
neigh:
        subq    $1, %rdx             # i - 1
        leaq    -1(%rcx), %r8        # j - 1
        addq    $1, %rcx             # j + 1
        imulq   %rsi, %rdx           # n(i - 1)
        leaq    (%rdx,%rsi,2), %rsi  # n(i - 1) + 2n = n(i + 1)
        leaq    (%rdx,%r8), %r9      # n(i - 1) + (j - 1)
        addq    %rcx, %rdx           # n(i - 1) + (j + 1)
        movq    (%rdi,%rdx,8), %rax  # res  = a[n(i - 1) + (j + 1)]
        movq    %rsi, %rdx           # rdx = n(i + 1)
        addq    (%rdi,%r9,8), %rax   # res += a[n(i - 1) + (j - 1)]
        subq    %rcx, %rsi           # n(i + 1) - (j + 1)
        subq    %r8, %rdx            # n(i + 1) - (j - 1)
        addq    (%rdi,%rdx,8), %rax  # res += a[n(i + 1) - (j - 1)]
        addq    (%rdi,%rsi,8), %rax  # res += a[n(i + 1) - (j + 1)]
        ret
```
#### Wersja w C

```c=
long neigh(long a[], long n, long i, long j) {
    long j_decr = j - 1;
    long j_incr = j + 1;
    long mult1 = n * (i - 1);
    long mult2 = mult1 + 2*n;     // n(i+1)
    long res = a[mult1 + j_incr];
    res     += a[mult1 + j_decr];
    res     += a[mult2 - j_decr];
    res     += a[mult2 - j_incr];
    return res;
}
```

Wyrażenia policzone teraz tylko raz to:
* `n(i-1)`
* `n(i+1)`
* `j-1`
* `j+1`

### Jak zoptymalizować to lepiej?

Zauważmy że jeśli opuścimy drugi nawias w tych wyrażeniach otrzymamy `coś +- 1`, a dodawanie/odejmowanie jedynki to jest przesunięcie adresu o stałą, czyli można to zrobić bezpośrednio w instrukcji odwołującej się do adresu. 

```c=
long neigh(long a[], long n, long i, long j) {
    long mult1 = n * (i - 1);
    long mult2 = mult1 + 2*n - j;     // n(i+1) - j
    mult1 += j;
    long res = a[mult1 - 1];
    res     += a[mult1 + 1];
    res     += a[mult2 + 1];
    res     += a[mult2 - 1];
    return res;
}
```

#### Kod wynikowy
```=
neigh:
        subq    $1, %rdx
        imulq   %rsi, %rdx
        leaq    (%rdx,%rcx), %r8
        leaq    (%rdx,%rsi,2), %rdx
        subq    %rcx, %rdx
        movq    8(%rdi,%r8,8), %rax
        addq    -8(%rdi,%r8,8), %rax
        addq    8(%rdi,%rdx,8), %rax
        addq    -8(%rdi,%rdx,8), %rax
        ret
```
## Zadanie 5

### Do czego służą programy profilujące?

Służą do sprawdzania ile czasu pochłaniają konkretne części programu w trakcie jego wykonywania. Przydają się one do namierzania miejsc w kodzie, które warto jest optymalizować.

### Profil płaski

Pokazuje on informacje o tym ile program spędził czasu w poszczególnych funkcjach. Przykład: 

![](https://i.imgur.com/WJslDjR.png)

### Profil grafu wywołań

Pokazuje ile czasu program spędził w danej procedurze oraz w każdej procedurze która została przez nią zawołana. Przykład:

![](https://i.imgur.com/98URST9.png)

Wyróżniona (z najmniejszym wcięciem) linia zawiera główną funkcję, wszystkie nad nią to funkcje wołające (rodzice), a poniżej funkcje wołane (dzieci).

Kolumny to: 

* `index` - numer procedury
* `% time` - procentowy czas jaki był spędzony w tej procedurze w stosunku do całości wliczając czas spędzony w funkcjach przez nią wołanych.
* `self` - całkowity czas spędzony wewnątrz procedury
* `children` - czas spędzony w funkcjach wołanych (dzieciach)
* `called` - licznik zawołań procedury, po '+' wymieniona jest liczba zawołań rekursywnych, w przypadku '/' są to dwie liczby, pierwsza oznacza liczbę wywołań z funkcji powyżej, a druga całkowitą liczbę wywołań nierekursywnych.
* `name` - nazwa procedury, której dotyczy linia oraz jej `index`.

### Dlaczego używamy opcji -pg?

Jest ona potrzebna żeby do kodu wynikowego na początku procedur dodane zostały dodatkowe instrukcje odkładające informacje np. na temat tego skąd została wywołana oraz o liczbie wywołań. Informacje te zostaną odczytane przez program profilujący.

### Zliczanie interwałowe

Oparta na nim jest implementacja zliczania czasu spędzonego w danej procedurze. Polega na przerwaniu wykonania programu w ustalonych, niewielkich, odstępach czasu. Ustalane jest wtedy, która procedura jest obecnie wykonywana a do jej licznika jest dodawana długość odstępu czasu. Jak widać może być to niedokładne, bo np. procedura może się zacząć i skończyć pomiędzy pomiarami, albo zacząć się bezpośrednio przed pomiarem, a i tak zostać obciążona czasem całego odstępu czasu.

### Przykład optymalizacji z podręcznika

Polegała ona na napisaniu naiwnej wersji programu, następnie identyfikowaniu fragmentów, w których spędzał on najwięcej czasu (tzw. *bottleneck'ów*).

Początkowy program zliczający wystąpienia *n-gramów* posiadał następujące etapy:

* Zamiana wszystkich liter na małe.
* Dla każdego n-gramu funkcja haszująca przypisuwała mu liczbę równą sumie kodów ASCII modulo `s`, żeby umieścić ją w tablicy z haszowaniem.
* Komórki tablicy są organizowane jako listy elementów, które przeszukujemy żeby zwiększyć ich licznik, lub dodajemy go na koniec, jeśli jeszcze nie ma go na liście. Operacja wykonywana jest rekursywnie.
* Sortowanie elementów tablicy wg liczby wystąpień wykonywane insert sortem.

Kolejne poczynione optymalizacje:

* insert sort -> biblioteczny quicksort (prawie 3m -> 4.7s)
* rekursywne przeglądanie listy -> iteracyjne ze wstawianiem na początek (4.7s -> 5.9s)
* iteracyjnie + wstawianie na początek -> wstawianie na koniec (5.9s -> 4.2s)
* zwiększenie tablicy z haszowaniem (4.2s -> 3.9)
* ulepszenie funkcji haszującej tak żeby pokrywała większy zakres wartości (3.9s -> 0.4s)
* ulepszenie algorytmu zmieniającego wielkość liter z kwadratowego na liniowy (0.4s -> 0.2s)

Największy efekt przyniosły zmiany algorytmów na posiadające mniejszą złożoność asymptotyczną (sortowanie, zamiana wielkości liter), oprócz nich ogromną poprawę złożoności przyniosło zmienienie funkcji haszującej, która to wynikała z obserwacji cechy danych na których pracujemy.

## Zadanie 8

![](https://i.imgur.com/EljzXx9.png)

### Kod wynikowy narzędzia llvm-mca

Uruchamiamy narzędzie `llvm-mca` z opcjami:

* `-mcpu-haswell` -- specyfikujemu mikroarchitekturę procesora
* `-timeline` -- dołączamy widok *timeline*.
* `-iterations=1` -- określamy liczbę iteracyjnych wywołań funkcji

#### Timeline view

```
Timeline view:
                    01234567
Index     0123456789        

[0,0]     DeER .    .    . .   subq	$1, %rsi
[0,1]     DeeeeeeeeER    . .   imulq	24(%rdi), %rsi
[0,2]     DeeeeeE---R    . .   movq	16(%rdi), %r8
[0,3]     .D=======eeeeeER .   movq	(%rdi,%rsi,8), %rax
[0,4]     .DeeeeeE-------R .   movq	32(%rdi), %rsi
[0,5]     .D============eER.   addq	$4, %rax
[0,6]     .D=====eE-------R.   leaq	(%rsi,%r8,8), %rsi
[0,7]     . D======eE-----R.   movq	%rsi, (%rdx)
[0,8]     . D============eER   movq	%rax, (%rcx)
[0,9]     .  DeeeeeeeE-----R   retq
```

W powyższym widoku zawarta jest graficzna reprezentacja stanu przetwarzania instrukcji w kolejnych cyklach. Użyte symbole oznaczają:

* `D` - wysłanie instrukcji
* `e` - instrukcja w trakcie wykonywania
* `E` - koniec wykonywania instrukcji
* `R` - zatwierdzenie instrukcji
* `=` - oczekiwanie na wykonanie instrukcji
* `-` - oczekiwanie na zatwierdzenie instrukcji

### Ile czasu zajmie wykonanie funkcji?

Możemy to odczytać z początku wydruku:

```
Total Cycles:      18
```

Wykonanie funkcji zajmie 18 cykli.

### Z czego wynikają opóźnienia?

#### Opóźnienie wykonania

Opóźnienia wykonania wynikają z tego że instrukcje muszą poczekać aż poprawne wartości ich operandów zostaną obliczone. Np. instrukcja
```
(%rsi,%r8,8), %rsi
```
Używa wartości z rejestrów `%rsi` i `%r8`, które wyliczają instrukcje `movq	32(%rdi), %rsi` i `movq	16(%rdi), %r8`, więc musi zaczekać aż zakończy się ich wykonywanie.

#### Opóźnienie zatwierdzenia

Wynika z tego, że instrukcje muszą być zatwierdzane w takiej samej kolejności jak w programie, czyli w tej samej kolejności zapisywać wartościami plik rejestru.

### Gdzie wystąpiło przezwanie rejestru?

Musiało ono nastąpić w instrukcji `movq	32(%rdi), %rsi`. Zauważmy, że zakończyła ona swoje wykonywanie przed instrukcją `movq	(%rdi,%rsi,8), %rax`, która korzysta z wartości znajdującej się w `%rsi`, mimo że znajduje się później na liście instrukcji. Gdyby jej wynik został skojarzony z rejestrem `%rsi` to poprzednia instrukcja otrzymałaby złą wartość. 

Przezwanie to pozwoliło wykonać szybciej nie tylko tą instrukcję, ale także te instrukcje znajdujące się dalej na liście, które były zależne od nadpisanej wartości i normalnie musiałyby czekać na zakończenie wykonywania instrukcji `movq 32(%rdi), %rsi`.
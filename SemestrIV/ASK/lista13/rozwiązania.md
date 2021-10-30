# ASK -- Lista 13

###### tags: `ASK`

## Zadanie 1

### Jak procesor przetwarza skoki warunkowe?

Jako że instrukcje są pobierane i przekazywane do potoku ze znacznym wyprzedzeniem, zanim znamy wyniki poprzednich instrukcji, pewny problem stanowią przy tym skoki warunkowe, gdyż nie wiemy gdzie przekazana zostanie kontrola po wykonaniu tej instrukcji, a więc jakie instrukcje przekazać do wykonania w dalszej kolejności.

#### Predykatory skoków

O tym, które instrukcje będą wykonane następnię decydują predykatory skoków, których zadaniem jest oszacować prawdopodobieństwo na wykonanie skoków i wybrać, które instrukcje powinny zostać załadowane do potoku. Są one dekodowana a nawet wykonywane (bez zapisu do pliku rejestrów). 

#### Co się dzieje gdy skok zostanie źle przewidziany?

Gdy predykator źle przewidzi wynik skoku, wiąże się to z pewnym kosztem, gdyż wszystkie wyniki instrukcji wykonanych spekulatywnie, a przed którymi wystąpiło złe przewidzenie skoku, muszą zostać porzucone, a przyniesienie z pamięci, zdekodowanie i wykonanie właściwych instrukcji, powoduje opóźnienie.

### Które skoki warto zoptymalizować do instrukcji cmov?

W szczególności te, które zależą od własności danych nie wykazujących wyraźnych wzorców, lub danych losowych, gdyż w przeciwnym wypadku predykatory skoków są na tyle dobre żeby w większości przypadków decydować poprawnie.

### Optymalizacja podanego kodu

```c=
void merge1(long src1[], long src2[], long dest[], long n) {
    long i1 = 0, i2 = 0;
    while (i1 < n && i2 < n)
        *dest++ = src1[i1] < src2[i2] ? src1[i1++] : src2[i2++];
}
```

#### Początkowy assembler

```=
merge1:
        testq   %rcx, %rcx
        jle     .L1
        movl    $0, %r8d
        movl    $0, %eax
        jmp     .L5
.L3:
        addq    $1, %r8           # albo dodajemy 1 do i2
        movq    %r10, %r9         # i przenosimy src2[i2] do r9
.L4:
        addq    $8, %rdx
        movq    %r9, -8(%rdx)     # bezwarunkowo przenosimy r9 pod *dst
        cmpq    %r8, %rax
        movq    %r8, %r9
        cmovge  %rax, %r9
        cmpq    %rcx, %r9
        jge     .L1
.L5:
        movq    (%rdi,%rax,8), %r9 # Obliczamy zarówno src1[i1]
        movq    (%rsi,%r8,8), %r10 # jak i src2[i2]
        cmpq    %r10, %r9          # w zależności od wyniku porównania
        jge     .L3
        addq    $1, %rax           # albo dodajemy 1 do i1
        jmp     .L4
.L1:
        rep ret
```

Widzimy że problemem nie jest przypisywanie wartości `src1[i1]`, ani `src2[i2]`, a warunkowe zwiększenie wartości `i1` lub `i2`. Możemy to ominąć wykorzystując działania na zmiennych logicznych.

#### Zoptymalizowana wersja

```c=
void merge1(long src1[], long src2[], long dest[], long n) {
    long i1 = 0, i2 = 0;
    while (i1 < n && i2 < n){
        bool cmp = src1[i1] < src2[i2];
        *dest++ = cmp ? src1[i1] : src2[i2];
        i1+=cmp;
        i2+=!cmp;
    }
}
```

#### Wynikowy assembler


```=
merge1:
        testq   %rcx, %rcx
        jle     .L1
        movl    $0, %r8d
        movl    $0, %eax
.L3:
        movq    (%rdi,%rax,8), %r10
        movq    (%rsi,%r8,8), %r9
        addq    $8, %rdx
        cmpq    %r9, %r10
        movq    %r9, %r11
        cmovle  %r10, %r11
        movq    %r11, -8(%rdx)
        setl    %r11b
        movzbl  %r11b, %r11d
        addq    %r11, %rax
        cmpq    %r9, %r10
        setge   %r9b
        movzbl  %r9b, %r9d
        addq    %r9, %r8
        cmpq    %r8, %rax
        movq    %r8, %r9
        cmovge  %rax, %r9
        cmpq    %rcx, %r9
        jl      .L3
.L1:
        rep ret
```

## Zadanie 2

Przypomnienie z poprzedniej listy:

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

### Przebieg eksperymentu z podręcznika

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
* ulepszenie funkcji haszującej na lepiej pokrywającą indeksy i bardziej wrażliwą (3.9s -> 0.4s)
* ulepszenie algorytmu zmieniającego wielkość liter z kwadratowego na liniowy (0.4s -> 0.2s)

#### Powtórzenie eksperymentu z podręcznika

##### Pierwsza iteracja

Parametry pozostają niezmienione (z tym, że w podręczniku początkowo użyta funkcja haszująca u nas jest oznaczona jako 1, a nie 0)

Wydruk profilu płaskiego:

```
Total time = 213.891441 seconds
gprof -b dictionary-pg gmon.out
Flat profile:

Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls   s/call   s/call  name
 97.18    202.32   202.32        1   202.32   202.32  sort_words
  2.85    208.25     5.94   965027     0.00     0.00  find_ele_rec
  0.07    208.40     0.15   965027     0.00     0.00  lower1
  0.03    208.47     0.07   965028     0.00     0.00  get_token
  0.03    208.53     0.06   965027     0.00     0.00  insert_string
  0.02    208.58     0.05   363039     0.00     0.00  save_string
  0.01    208.60     0.02   965027     0.00     0.00  h_add
  0.00    208.61     0.01   363039     0.00     0.00  new_ele
  0.00    208.61     0.00   965029     0.00     0.00  get_word
  0.00    208.61     0.00        8     0.00     0.00  find_option
  0.00    208.61     0.00        7     0.00     0.00  add_int_option
  0.00    208.61     0.00        1     0.00     0.00  add_string_option
  0.00    208.61     0.00        1     0.00     0.00  parse_options
  0.00    208.61     0.00        1     0.00     0.00  show_options
  0.00    208.61     0.00        1     0.00   208.61  word_freq
```

Prawie cały czas został spędzony w procedurze sortowania, więc ona zostanie zoptymalizowana jako pierwsza.

##### Druga iteracja

`QUICK = 0 -> QUICK = 1`

insert sort -> biblioteczny quicksort (prawie 3.5m -> 10.6s)

Wydruk profilu płaskiego

```
Total time = 10.860269 seconds
gprof -b dictionary-pg gmon.out
Flat profile:

Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls   s/call   s/call  name
 93.47      5.01     5.01   965027     0.00     0.00  find_ele_rec
  4.49      5.25     0.24   965027     0.00     0.00  lower1
  0.93      5.30     0.05                             compare_ele
  0.56      5.33     0.03   363039     0.00     0.00  new_ele
  0.56      5.36     0.03        1     0.03     0.03  sort_words
  0.19      5.37     0.01   965028     0.00     0.00  get_token
  0.00      5.37     0.00   965029     0.00     0.00  get_word
  0.00      5.37     0.00   965027     0.00     0.00  h_add
  0.00      5.37     0.00   965027     0.00     0.00  insert_string
  0.00      5.37     0.00   363039     0.00     0.00  save_string
  0.00      5.37     0.00        8     0.00     0.00  find_option
  0.00      5.37     0.00        7     0.00     0.00  add_int_option
  0.00      5.37     0.00        1     0.00     0.00  add_string_option
  0.00      5.37     0.00        1     0.00     0.00  parse_options
  0.00      5.37     0.00        1     0.00     0.00  show_options
  0.00      5.37     0.00        1     0.00     5.32  word_freq
```

Procedura sortowania przestała dominować w całościowym czasie wykonywania programu, kolejnym problemem jest czas spędzany na przeszukiwaniu list w tablicy haszującej. Spróbujemy zastąpić rekursywną funkcję `find_ele_rec` jej iteracyjną wersją.

##### Trzecia iteracja

`FIND = 0 -> FIND = 1`

rekursywne przeglądanie listy -> iteracyjne ze wstawianiem na początek (10.6s -> 13.2s)

Wydruk profilu płaskiego:
```
Total time = 13.218148 seconds
gprof -b dictionary-pg gmon.out
Flat profile:

Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls   s/call   s/call  name
 95.39      8.15     8.15   965027     0.00     0.00  find_ele_iter_f
  2.93      8.40     0.25   965027     0.00     0.00  lower1
  0.47      8.44     0.04        1     0.04     0.04  sort_words
  0.35      8.47     0.03   965028     0.00     0.00  get_token
  0.23      8.49     0.02   965027     0.00     0.00  h_add
  0.23      8.51     0.02   965027     0.00     0.00  insert_string
  0.23      8.53     0.02   363039     0.00     0.00  new_ele
  0.23      8.55     0.02                             compare_ele
  0.12      8.56     0.01   363039     0.00     0.00  save_string
  0.00      8.56     0.00   965029     0.00     0.00  get_word
  0.00      8.56     0.00        8     0.00     0.00  find_option
  0.00      8.56     0.00        7     0.00     0.00  add_int_option
  0.00      8.56     0.00        1     0.00     0.00  add_string_option
  0.00      8.56     0.00        1     0.00     0.00  parse_options
  0.00      8.56     0.00        1     0.00     0.00  show_options
  0.00      8.56     0.00        1     0.00     8.54  word_freq
```

Tak samo jak w doświadczeniu opisanym w podręczniku, czas się pogorszył. Jest to spowodowane tym, że iteracyjna wersja umieszcza nowe elementy na początku, a przy równomiernym rozkładzie n-gramów, oczekujemy, że te częściej występujące pojawią się w tekście wcześniej i dlatego znajdą się na końcu list, co jest niepożądane.

##### Czwarta iteracja

`FIND = 1 -> FIND = 2`

iteracyjnie + wstawianie na początek -> wstawianie na koniec (13.2s -> 9.4s)

Wydruk profilu płaskiego:

```
Total time = 9.399526 seconds
gprof -b dictionary-pg gmon.out
Flat profile:

Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls   s/call   s/call  name
 94.54      5.69     5.69   965027     0.00     0.00  find_ele_iter_r
  3.00      5.87     0.18   965027     0.00     0.00  lower1
  0.67      5.91     0.04   965028     0.00     0.00  get_token
  0.67      5.95     0.04                             compare_ele
  0.50      5.98     0.03        1     0.03     0.03  sort_words
  0.33      6.00     0.02   965027     0.00     0.00  h_add
  0.33      6.02     0.02   363039     0.00     0.00  new_ele
  0.17      6.03     0.01   965027     0.00     0.00  insert_string
  0.00      6.03     0.00   965029     0.00     0.00  get_word
  0.00      6.03     0.00   363039     0.00     0.00  save_string
  0.00      6.03     0.00        8     0.00     0.00  find_option
  0.00      6.03     0.00        7     0.00     0.00  add_int_option
  0.00      6.03     0.00        1     0.00     0.00  add_string_option
  0.00      6.03     0.00        1     0.00     0.00  parse_options
  0.00      6.03     0.00        1     0.00     0.00  show_options
  0.00      6.03     0.00        1     0.00     5.99  word_freq
```

Udało nam się nieznacznie polepszyć czas w stosunku do przeszukiwania rekurencyjnego, jednak wciąż to przeszukiwanie list trwa najdłużej, należy zastanowić się zatem nad własnościami naszej tablicy haszującej. W pierwszej kolejności zwiększymy liczbę list, żeby potencjalnie zmniejszyć średnią liczbę elementów na listę.

##### Piąta iteracja

`SIZE = 1021 -> SIZE = 199 999`

zwiększenie tablicy z haszowaniem (9.4s -> 8.1)

Wydruk profilu płaskiego:

```
Total time = 8.130678 seconds
gprof -b dictionary-pg gmon.out
Flat profile:

Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls   s/call   s/call  name
 92.96      4.89     4.89   965027     0.00     0.00  find_ele_iter_r
  4.57      5.13     0.24   965027     0.00     0.00  lower1
  0.76      5.17     0.04   965028     0.00     0.00  get_token
  0.76      5.21     0.04        1     0.04     0.04  sort_words
  0.38      5.23     0.02   965027     0.00     0.00  h_add
  0.38      5.25     0.02   363039     0.00     0.00  save_string
  0.19      5.26     0.01   363039     0.00     0.00  new_ele
  0.19      5.27     0.01                             h_xor
  0.00      5.27     0.00   965029     0.00     0.00  get_word
  0.00      5.27     0.00   965027     0.00     0.00  insert_string
  0.00      5.27     0.00        8     0.00     0.00  find_option
  0.00      5.27     0.00        7     0.00     0.00  add_int_option
  0.00      5.27     0.00        1     0.00     0.00  add_string_option
  0.00      5.27     0.00        1     0.00     0.00  parse_options
  0.00      5.27     0.00        1     0.00     0.00  show_options
  0.00      5.27     0.00        1     0.00     5.26  word_freq
```

Zysk jest, ale niewielki jak na tak znaczącą zmianę stosunku liczby elementów do liczby list. Po głębszej analizie okazuje się, że nasza funkcja haszująca jest beznadziejna, gdzyż nie jest wrażliwa np. na zmianę kolejności liter oraz średnio jest w stanie wygenerować dla tych danych tylko klucze z bardzo wąskiego zakresu w stosunku do wielkości tablicy. W kolejnym kroku zmienimy ją na lepiej mieszającą bity i pokrywającą tablicę.

##### Szósta iteracja

`HASH = 1 -> HASH = 2`

ulepszenie funkcji haszującej na lepiej pokrywającą indeksy i bardziej wrażliwą (8.1s -> 0.7s)

```
Total time = 0.733559 seconds
gprof -b dictionary-pg gmon.out
Flat profile:

Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls  ms/call  ms/call  name
 38.71      0.17     0.17   965027     0.00     0.00  lower1
 25.05      0.28     0.11   965027     0.00     0.00  find_ele_iter_r
 15.94      0.35     0.07   965028     0.00     0.00  get_token
 11.39      0.40     0.05   965027     0.00     0.00  insert_string
  4.55      0.42     0.02                             compare_ele
  2.28      0.43     0.01        1    10.02    10.02  sort_words
  2.28      0.44     0.01                             find_ele_iter_f
  0.00      0.44     0.00   965029     0.00     0.00  get_word
  0.00      0.44     0.00   965027     0.00     0.00  h_xor
  0.00      0.44     0.00   363039     0.00     0.00  new_ele
  0.00      0.44     0.00   363039     0.00     0.00  save_string
  0.00      0.44     0.00        8     0.00     0.00  find_option
  0.00      0.44     0.00        7     0.00     0.00  add_int_option
  0.00      0.44     0.00        1     0.00     0.00  add_string_option
  0.00      0.44     0.00        1     0.00     0.00  parse_options
  0.00      0.44     0.00        1     0.00     0.00  show_options
  0.00      0.44     0.00        1     0.00   410.83  word_freq
```

Zmiana ta miała ogromny wpływ na czas wykonywania programu, obecnie nieznacznie dominującą funkcją jest `lower1` konwertująca wielkie litery na małe, spowodowane jest to jej kwadratową złożonością w stosunku do długości napisów ze względu na wywołanie `Strlen` przy każdym sprawdzeniu warunku w pętli.

##### Siódma iteracja

`LOWER = 0 -> LOWER = 1`

ulepszenie algorytmu zmieniającego wielkość liter z kwadratowego na liniowy (0.7s -> 0.5s)

```
Total time = 0.505367 seconds
gprof -b dictionary-pg gmon.out
Flat profile:

Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls  ms/call  ms/call  name
 35.79      0.05     0.05   965027     0.00     0.00  find_ele_iter_r
 28.63      0.09     0.04   965028     0.00     0.00  get_token
 14.31      0.11     0.02   965027     0.00     0.00  lower2
  7.16      0.12     0.01   965027     0.00     0.00  h_xor
  7.16      0.13     0.01   363039     0.00     0.00  save_string
  7.16      0.14     0.01        1    10.02    10.02  sort_words
  0.00      0.14     0.00   965029     0.00     0.00  get_word
  0.00      0.14     0.00   965027     0.00     0.00  insert_string
  0.00      0.14     0.00   363039     0.00     0.00  new_ele
  0.00      0.14     0.00        8     0.00     0.00  find_option
  0.00      0.14     0.00        7     0.00     0.00  add_int_option
  0.00      0.14     0.00        1     0.00     0.00  add_string_option
  0.00      0.14     0.00        1     0.00     0.00  parse_options
  0.00      0.14     0.00        1     0.00     0.00  show_options
  0.00      0.14     0.00        1     0.00   140.28  word_freq
```

Rezultat jest widoczny, procedura `lower` w wersji drugiej nie dominuje już w sumarycznym czasie działania programu. I na tym kończymy przeprowadzone optymalizacje.
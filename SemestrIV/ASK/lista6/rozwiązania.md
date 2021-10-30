# ASK -- Lista 6

###### tags: `ASK`

## Zadanie 1

Zgodnie z *System V ABI* argumenty funkcji będą dane w rejestrach:

* `RDI` -- argument `n`
* `RSI` -- argument `*p`

Zwracamy wynik przez rejest `EAX`.

```=
puzzle:
        push %rbp              # odkładamy %rbp na stos 
        xorl %eax, %eax        # zerujemy result
        mov %rsi, %rbp         # zachowujemy *p
        push %rbx              # odkładamy %rbx na stos
        mov %rdi, %rbx         # zachowujemy wartość n
        sub $24, %rsp          # rezerwujemy 24 (3 razy 8) bajty
        test %rdi, %rdi        # SF=MSB(n&n), OF=0, jeśli n=0 to ZF=1
        jle .L1                # Skocz jeśli n<=0 (ZF=1 lub SF!=OF)
        lea 8(%rsp), %rsi      # Zapisujemy w rsi adres drugiej zmiennej 
                               # lokalnej ze stosu (tmp) 
        lea (%rdi,%rdi), %rdi  # nowe n = n + n (n << 1)
        call puzzle            # wywołujemy n z parametrami n << 1, &tmp
        add 8(%rsp), %rax      # result += tmp
        add %rax, %rbx         # n += tmp
.L1:    mov %rbx, (%rbp)       # *p = n
        add $24, %rsp          # zwalniamy zarezerwowane miejsce na stosie
        pop %rbx               # przywracamy wartość rbx
        pop %rbp               # i rbp
        ret
```

## Kod w języku C

```c=
long puzzle(long n, long *p){
    long result = 0;
    long tmp;
    if(n > 0){
        result = puzzle(n << 1, &tmp);
        result += tmp;
        n += result;
    }
    *p = n;
    return result;
}
```
## Zawartość rekordu aktywacji

| Lp. | Adres    |   Zawartość   |
|- | -------- |:-------------:|
|1| 0x(...)8 | adres powrotu |
|2| 0x(...)0 |     %rbp      |
|3| 0x(...)8 |     %rbx      |
|4| 0x(...)0 |     pusty     | 
|5| 0x(...)8 |      tmp      |
|6| 0x(...)0 |     pusty     |

**Rejestry zapisane przez funkcję wołaną**, czyli rejestry, których stan nie zostać naruszony (czyli musi zostac odtworzony w razie potrzeby) przez funkcję wołaną: 2,3

**Zmienne lokalne**: 4,5,6

**Adres powrotu**: 1

## Zadanie 2

Zgodnie z *System V ABI*, ponieważ wartość zwracana nie mieści się w dwóch rejestrach, to będziemy musieli zwrócić ją przez pamięć. W tym przypadku funkcja wołająca musi zapewnić zarezerwowaną pamięć na umieszczenie zwracanej wartości oraz wskaźnik na nią, jako dodatkowy, ukryty, pierwszy argument funkcji wołanej. Wskaźnik ten będzie faktyczną zwracaną wartością przez rejestr %rax. Zatem argumenty funkcji znajdą się w rejestrach:

* `RDI` -- ukryty argument `*s` typu `T*` 
* `RSI` -- argument `*a`
* `RDX` -- argument `n`


```
puzzle8:
        movq %rdx, %r11            # r11 = n
        xorl %r10d, %r10d          # zerujemy r10 (od teraz i)
        xorl %eax, %eax            # zerujemy rax (od teraz sum)
        movq $LONG_MIN, %r8        # max = long_min
        movq $LONG_MAX, %r9        # min = long_max
.L2:    cmpq %r11, %r10            #
        jge .L5                    # skaczemy jeśli i >= n
        movq (%rsi,%r10,8), %rcx   # pobieramy i-ty element z a
        cmpq %rcx, %r9             # porównujemy z min
        cmovg %rcx, %r9            # jeśli min większy to min = a[i]
        cmpq %rcx, %r8             # porównujemy z max
        cmovl %rcx, %r8            # jeśli max mniejszy to max = a[i]
        addq %rcx, %rax            # sum+=a[i]
        incq %r10                  # i++
        jmp .L2                    # skaczemy na początek pętli
.L5:    cqto                       # sign-extend RAX -> RDX:RAX
        movq %r9, (%rdi)           # s.min = min
        idivq %r11                 # dzielimy RDX:RAX(sum) przez n
        movq %r8, 8(%rdi)          # s.max = max
        movq %rax, 16(%rdi)        # s.average = sum / n
        movq %rdi, %rax            # return s*
        ret
```

### Odpowiednik w C

#### Definicja structa

```c=
struct T {
    long min;
    long max;
    long average;
};
```

#### Kod procedury

```c=
struct T puzzle8(long *a, long n){
    long sum = 0;
    long max = LONG_MIN;
    long min = LONG_MAX;
    
    for(int i = 0; i < n; i++){
        long elem = a[i];
        if(elem > max) max = elem;
        if(elem < min) min = elem;
        sum += elem;
    }
    
    return (struct T) {.min = min, .max = max, average = sum / n};
}

```

### Co robi ta procedura?

Znajduje jednocześnie największy, najmniejszy i średnią z n-elementowej tablicy `a` i zapisuje wynik w struct'cie.

### Jak mogłaby wyglądać sygnatura?

Z kodu należałoby wywnioskować sygnaturę `struct T* puzzle(struct T *s, long *a, long n)`.

## Zadanie 4

Zgodnie z *System V ABI* argumenty funkcji będą dane w rejestrach:

* `RDI` -- argument `n`

Zwracamy wynik przez rejest `RAX`.

```=
M:      
        pushq %rdi            #odkładamy argument n na stos
        testq %rdi, %rdi      
        je .L2                # jeśli n==0 (ZF == 1) skaczemy do return n
        leaq -1(%rdi), %rdi   # nowy argument = n-1
        call M                # tmp = M(n-1)
        movq %rax, %rdi       # wynik będzie argumentem kolejnego wywołania
        call F                # tmp2 = F(tmp)
        movq (%rsp), %rdi     # odtwarzamy n
        subq %rax, %rdi       # n = n - tmp2
.L2:    movq %rdi, %rax       # zwracamy n
        ret
    
F: 
        testq %rdi, %rdi      
        je .L3                # skocz jeśli n==0
        movq %rdi, %r12       # przechowujemy n w r12
        leaq -1(%rdi), %rdi   # nowy argument = n-1
        call F                # tmp = F(n-1)
        movq %rax, %rdi       # wynik będzie argumentem kolejnego wywołania
        call M                # tmp2 = M(tmp)
        subq %rax, %r12       # n = n - tmp2
        movq %r12, %rax       # zwracamy n
        ret
L3:     movl $1, %eax         # zwróć 1
        ret
```
### Błąd #1

W funkcji `M` odkładamy wartość na stos i jej z niego nie zdejmujemy, przez co adres powrotu z funkcji będzie błędny (wczytane zostanie n). Poprawka:

```=
M:      pushq %rdi
        testq %rdi, %rdi      
        je .L2
        leaq -1(%rdi), %rdi
        call M
        movq %rax, %rdi
        call F
        movq (%rsp), %rdi
        subq %rax, %rdi
.L2:    movq %rdi, %rax
        popq %rdi          # < (albo sub $8 %rsp) 
        ret
```

### Błąd #2

W funkcji `F` używamy rejestru `%r12` w celu przechowania wartości zmiennej `n` na czas innych wywołań. Możemy to zrobić, gdyż jest to jeden z rejstrów *calle-saved*, lecz musimy też zadbać o to żeby jego wartość z funkcji wołającej została zachowana i odtworzona. Poprawka:

```=
F:      testq %rdi, %rdi      
        je .L3
        pushq %r12            #<---
        movq %rdi, %r12
        leaq -1(%rdi), %rdi
        call F
        movq %rax, %rdi
        call M
        subq %rax, %r12
        movq %r12, %rax
        popq %r12             #<---
        ret
L3:     movl $1, %eax
        ret
```

### Błąd #3

W oryginalnym kodzie, w funkcji `F` przed zawołaniem innych procedur wskaźnik na szczyt stosu nie został wyrównany do wartości podzielnej przez 16, problem ten został jednak rozwiązany w powyższej poprawce.

### Odpowiednik w C (bo czemu nie)

```c=
long M(long n){
    if(n==0) return 0;
    return n - F(M(n-1));
}

long F(long n){
    if(n==0) return 1;
    return n - M(F(n-1));
}

```

Jest to defincja ciągu *Female and Male* Hofstadtera.

## Zadanie 5

**Jednostka translacji** - plik źródłowy plus pliki dołączone przez `#include` minus ewentualne pliki wyłączone przez dyrektywy warunkowe.

**Alloca(3)** Rezerwuje miejsce wewnątrz ramki stosu i automatycznie je zwalnia po zakończeniu wywołania funkcji w której została wywołana. Zwraca wskaźnik na początek zarezerwowanego miejsca.

### Wynik deasemblacji

Argument `n` trafia do nas w rejestrze `RDI`, `idx` w `RSI` i `*q` w `RDX`.

```=
00000000000005fa <aframe>:
               # początek alloca
 5fa:   push   %rbp                       # zachowujemy poprzednią wartość
                                          # rejestru rbp
 5fb:   mov    %rsp,%rbp                  # nowym końcem jest rsp
 5fe:   sub    $0x10,%rsp                 # rezerwujemy 2 bajty na stosie
 602:   lea    0x0(,%rdi,8),%r9           # r9 = n * sizeof(long*)
 60a:   lea    0x1e(%r9),%rax             # obliczanie ile bajtów -
 60e:   and    $0xfffffffffffffff0,%rax   # - zarezerwować
 612:   sub    %rax,%rsp                  # zarezerwowanie pamięci
 
 615:   lea    0xf(%rsp),%r8              # obliczanie wskaźnika na -
 61a:   and    $0xfffffffffffffff0,%r8    # - zarezerwowane miejsce
 61e:   mov    %r8,%rcx                   # wynik(wskaźnik) zapisujemy w p
               #koniec alloca
 
 621:   lea    -0x8(%rbp),%rax            # &i
 625:   mov    %rax,-0x8(%r8,%r9,1)       # p[n-1] = &i
 62a:   mov    $0x0,%eax                  # i = 0
 62f:   jmp    639 <aframe+0x3f>          # skocz do warunku
 631:   mov    %rdx,(%rcx,%rax,8)         # p[i] = q
 635:   add    $0x1,%rax                  # i++
 639:   cmp    %rdi,%rax                  # i < n
 63c:   jl     631 <aframe+0x37>          # pętlenie się
 63e:   mov    (%r8,%rsi,8),%rax          # p[idx]
 642:   mov    (%rax),%rax                # *p[idx]
 
 645:   leaveq                            # zwolnienie miejsca ze stosu
 646:   retq
```

Istrukcja `leaveq` ustawia wartość `RSP` na poprzednią (przechowywaną teraz w `RBP` przez instrukcję `mov %rsp, %rbp`) i resetuje rejestr `RBP`, którego wcześniejsza wartość znajduje się teraz na szczycie (wcześniej umieszczona tam przez `pushq rbp`).

## Zadanie 6

Zgodnie z *System V ABI* wynik procedury jest zwracany przez rejestr `RAX`.

```=
puzzle5:
        subq $24, %rsp       # rezerwujemy 3*8 bajtów w pamięci (trzy zmienne)
        movq %rsp, %rdi      # wskaźnik na pierwszą przekazujemy do readlong
        call readlong        # readlong(x)
        leaq 8(%rsp), %rdi   # wskaźnik na drugą przekazujemy do readlong
        call readlong        # readlong(y)
        movq (%rsp), %rax    # rax = x
        cqto                 # sign-extend RAX -> RDX:RAX
        idivq 8(%rsp)        # rax = x / y, rdx = x % y
        xorl %eax, %eax      # zerujemy rax
        testq %rdx, %rdx     
        sete %al             # jeśli rdx == 0 (ZF==1) rax = 1, wpp rax = 0
        addq $24, %rsp       # zwalniamy miejsce ze stosu
        ret
```

### Odpowiednik w C

Sygnatura readlong to zapewne `void readlong(long *x)`. Wynik wczytanej wartości przekazuje przez argument.

```c=
long puzzle5(){
    long x,y;
    
    readlong(&x);
    readlong(&y);
    
    return x % y == 0;
}
```

### Rekord aktywacji procedury

| Zawartość     | Rozmiar w bajtach |
| ------------- | ----------------- |
| adres powrotu | 8                 |
| nieużyty      | 8 |
| y | 8 |
| x | 8 |

Rozmiar rekordu: 32 bajty

### Co robi podana procedura?

Sprawdza czy pierwsza wczytana liczba jest podzielna przez drugą.

## Zadanie 7

Żeby użyć listy argumentów, najpierw musimy zadeklarować zmienną typu `va_list`, która jest listą zawierającą pojedynczy struct, w którym znajdą się wszystkie informacje potrzebne do użycia `va_arg`, następnie zainicjować ją za pomocą makra `va_start`. Na koniec koniecnie jest użycie makra `va_end`, które przeprowadzi wszystkie konieczne akcje przed returnem.

### Definicja va_list

Zgodnie z *System V ABI* jest to

```c=
typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} va_list[1];
```

Gdzie znaczenie pól jest zdefiniowane następująco:

* `gp_offset` - odległość w bajtach od początku `register save area` do miejsca gdzie zapisany został kolejny argument, który był przekazany przez rejestr. Jeśli wszystkie zostały wyczerpane, ma wartość 48.
* `fp_offset` - to samo co powyżej, tylko dotyczy rejestrów zmiennoprzecinkowyć. Jeśli wszystkie zostały wyczerpane ma wartość 304.
* `overflow_arg_area` - zawsze wskazuje na kolejny argument z tych przekazanych na stosie
* `reg_save_area` - wskazuje na początek miejsca zapisanego argumentami przekazanymi przez rejestry


### Kod procedury

```=
puzzle7:
        movq %rsi, -40(%rsp)    # w  tych linijkach  kopiujemy -
        movq %rdx, -32(%rsp)    # - wartości argumentów, które -
        movq %rcx, -24(%rsp)    # - były   przekazane    przez -
        movq %r8, -16(%rsp)     # - rejestry, do register save -  
        movq %r9, -8(%rsp)      # - area
        movl $8, -72(%rsp)      # ustawiamy gp_offset na 8, zapisujemy na stosie
        leaq 8(%rsp), %rax      # wyliczamy *overflow_arg_area
        movq %rax, -64(%rsp)    # zapisujemy na stosie
        leaq -48(%rsp), %rax    # wyliczamy *reg_save_area
        movq %rax, -56(%rsp)    # zapisujemy na stosie
        movl $0, %eax           # zerujemy rax (od teraz result)
        jmp .L2
.L3:    movq -64(%rsp), %rdx    # rdx = overflow_arg_area
        leaq 8(%rdx), %rcx      # zwiększamy overflow_arg_area o 8
        movq %rcx, -64(%rsp)    # i zapisujemy
.L4:    addq (%rdx), %rax       # zwiększamy result o wartość k. argumentu
.L2:    subq $1, %rdi           # rdi (od teraz n) zmniejszamy o 1   
        js .L6                  # skocz jeśli SF==1 (koniec arg. z rej.)
        cmpl $47, -72(%rsp)
        ja .L3                  # jeśli gp_offset > 47, skaczemy do L3  
        movl -72(%rsp), %edx    # wpp. edx = gp_offset
        addq -56(%rsp), %rdx    # gp_offset += reg_save_area
        addl $8, -72(%rsp)      # zwiększamy offset od 8
        jmp .L4
.L6:    ret
```

### Odpowiednik w C

Funkcja będzie z dużym prawdopodobieństwiem mieć sygnaturę `long sum(long n, ...)`.

```c=
long sum(long n, ...){
    long result = 0;
    va_list args;
    va_start(args, n);
    
    for(; n > 0; n--){
        result += va_arg(args, long);
    }
    
    va_end(args);
    return result;
}
```

Funkcja ta zwraca sumę podanych do niej argumentów.

### Stan rekordu aktywacji procedury

|   Adres   |      Zawartość      |                   Uwagi                   |
|:---------:|:-------------------:|:-----------------------------------------:|
|    ...    |         ...         |                    ...                    |
|  8(%rsp)  |        arg1         |        początek overflow_arg_area         |
|   %rsp    |    adres powrotu    |                                           |
| -8(%rsp)  |         R9          | ostatni argument przekazany przez rejestr |
| -16(%rsp) |         R8          |                                           |
| -24(%rsp) |         RCX         |                                           |
| -32(%rsp) |         RDX         |                                           |
| -40(%rsp) |         RSI         |                                           |
| -48(%rsp) |     niezapisany     |        początek register-save-area        |
| -56(%rsp) |   \*reg_save_area   |       w naszym przypadku -48(%rsp)        |
| -64(%rsp) | \*overflow_arg_area |            na początku 8(%rsp)            |
| -72(%rsp) |      gp_offset      |               na początku 8               |
|    ...    |         ...         |                    ...                    |
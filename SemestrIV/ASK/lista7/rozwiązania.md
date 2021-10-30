# ASK - Lista 7
###### tags: `ASK`

## Zadanie 1

Mamy daną definicję dwóch structów i kod, w którym modyfikowane są wartości ich pól, musimy wydedukować wartości stałych `A` i `B`.

### Definicja structów

#### Definicja str1
```c=
typedef struct {
    int x[A][B];
    long y;
} str1;
```
#### Definicja str2
```c=
typedef struct {
    char array[B];
    int t;
    short s[A];
    long u;
} str2;
```
### Kod modyfikujący w C
```c=
void set_val(str1 *p, str2 *q) {
    long v1 = q->t;
    long v2 = q->u;
    p->y = v1 + v2;
}
```
### Kod modyfikujący w asemblerze

Zmienne `p` i `q` otrzymujemy w rejestrach `RDI` i `RSI` odpowiednio.

```=
set_val:
    movslq 8(%rsi),%rax
    addq 32(%rsi),%rax
    movq %rax,184(%rdi)
    ret
```

Pierwszą obserwacją jaką możemy poczynić to że w lini 4 dostęp do zmiennej `y` jest robiony przez `184(%rdi)`, czyli tablica `x[A][B]` ma rozmiar mniejszy, lub równy 184-rem bajtom, ale większy niż 176. 
$$
176 < 4 \cdot AB \leq 184
$$

Czyli
$$
    44 < AB \leq 46
$$

Kolejna rzecz to dostęp do `t` jest robiony przez `8(%rsi)`, czyli rozmiar `array[B]` jest mniejszy lub równy 8, ale większy niż 4, czyli 
$$
    4<B\leq 8
$$

Na koniec wykorzystamy fakt, że dostęp do pola `u` jest wykonany przez
`32(%rsi)`, póki co wiemy, że

* `t` zaczyna się na offsecie 8
* nie ma paddingu między `t`, a `s`, bo `s` ma typ o mniejszym rozmiarze

Czyli tablica `s` zaczyna się na offsecie 12, a kończy najdalej na 32, ale może mieć padding aż 7-bajtowy, czyli może się kończyć już na offsecie 18.
$$
    6 < A \leq 10
$$

Czyli ostatecznie wiemy, że $A$ musi być ze zbioru $\{7,6,8,9,10\}$, $B$ z $\{5,6,7,8\}$ i $AB$ z $\{45,46\}$. Zatem widać, że możliwymi kandydatami na wartości tych stałych są $A = 9$ i $B = 5$.

## Zadanie 2

Mamy podany kod w C i asemblerze odwołujący się do elementu tablicy trójwymiarowej `long A[R][S][T]`. Przypomnijmy że taka tablica będzie ułożona w pamięci w taki sposób:
```
|--------------- R ---------------|
|--- S ---| |--- S ---| |--- S ---|
|T| |T| |T| |T| |T| |T| |T| |T| |T|
```

Czyli żeby odwołać się do tablicy `[S][T]` o indeksie $i$ będziemy musieli przeskoczyć $|S| \cdot |T| \cdot i$ longów, wtedy znajdziemy się na jej początku. Następnie żeby odwołać się do tablicy `[T]` o indeksie $j$ będziemy musieli przeskoczyć $|T| \cdot j$ longów. Na koniec żeby odwołać się do konkretnego longa, będziemy musieli przesunąć się o $k$ longów. Interesować będzie nas zatem przez co zostały wymnożone indeksy w kodzie asemblerowym.

### Kod w C

```c=
long A[R][S][T];

long store_elem(long i, long j, long k, long *dest){
    *dest = A[i][j][k];
    return sizeof(A);
}
```

### Kod w asemblerze

Argumenty funkcji `i,j,k,dest` otrzymujemy w rejestrach `RDI`, `RSI`, `RDX` i `RCX`. 

```=
store_elem:
    leaq (%rsi,%rsi,2),%rax    # rax = 3*j
    leaq (%rsi,%rax,4),%rax    # rax = 13*j
    movq %rdi,%rsi             # rsi = i
    salq $6,%rsi               # rsi = 64*i
    addq %rsi,%rdi             # rsi = 65*i
    addq %rax,%rdi             # rdi = 65*i + 13*j
    addq %rdi,%rdx             # rdx = 65*i + 13*j + k
    movq A(,%rdx,8),%rax       # *(A + 8(65*i + 13*j + k)
    movq %rax,(%rcx)           # *dest = A[i][j][k]
    movq $3640,%rax            # return sizeof(A)
    ret
```

Na podstawie wcześniejszego opisu może wywnioskować przede wszystkim, że $T = 13$, zatem $S = \frac{65}{13} = 5$, a skoro `sizeof(A) = 3640`, to całość musi zawierać 455 longów, zatem $R = \frac{455}{13 \cdot 5} = 7$.

## Zadanie 3

### Kod w języku C

```c=
typedef struct {
    int first;
    a_struct a[CNT];
    int last;
} b_struct;

void test (long i, b_struct *bp) {
    int n = bp->first + bp->last;
    a_struct *ap = &bp->a[i];
    ap->x[ap->idx] = n;
}
```

### Kod w asemblerze
Argumenty procedury czyli `i` i `bp` otzymujemy w rejestrach `RDI` i `RSI`.
```=
test:
    movl 0x120(%rsi),%ecx       # bp->last
    addl (%rsi),%ecx            # n = bp->first + bp->last
    leaq (%rdi,%rdi,4),%rax     # rax = 5i
    leaq (%rsi,%rax,8),%rax     # rax = bp + 40i
    movq 0x8(%rax),%rdx         # *(rax + 8), czyli to ap->idx
    movslq %ecx,%rcx            # sign extend n (czyli elementy x są większe)
    movq %rcx,0x10(%rax,%rdx,8) # *(8 + (8 + (8 * ap->idx)) + rax) = n
    retq
```

### Wartość CNT

Z lini drugiej jesteśmy w stanie wywynioskować, że pole `last` znajduje się pod offsetem 288.

```
|first|_____________________________|last|
      |                             |
     +4                            +288
               b_struct
```

Żeby dostać się do $i$-tego structa mnożymy $i$ przez 40, zatem `sizeof(struct_a) == 40`. Z lini 6 możemy również wywnioskować, że tablica structów umieszczona jest pod offsetem 8, czyli istnieje 4 bajtowy padding po polu `first`. Zatem structy są wyrównane do adresów podzielnych przez 8, więc na pewno nie będzie paddingu przed polem `last`, bo jest ono 4 bajtowe.

```
|first|###|_________a[CNT]__________|last|
      |   |                         |
     +4  +8                        +288
                 b_struct
```
Znając wielkość pojedynczego structa i ile bajtów zajmuje tablica `a[CNT]` możemy wywnioskować wartość `CNT`.
$$
    CNT = \frac{280}{40} = 7
$$

### Definicja a_struct

Z lini 6 jesteśmy także w stanie wywnioskować, że pole `idx` znajduje się na początku structa i ma rozmiar ośmiu bajtów. Podobnie z ósmej możemy wydedukować że tablica `x[]` znajduje się pod offsetem 8 i również jest tablicą 8 bajtowych wartości, czyli przy założeniu, że w structcie nie ma innych pól, to nie ma też w nim paddingów, więc tablica jest 32-bajtowa, więc 4-elementowa.

```c=
typedef struct {
    long idx;
    long x[4];
} a_struct
```

## Zadanie 4

### Definicja unii

```c=
union elem {
    struct {
        long *p;
        long y;
    } e1;
    
    struct {
        long x;
        union elem *next;
    } e2;
};
```

Jej rozmiar będzie równy rozmiarowi największego składnika, w tym przypadku oba składniki mają 16 bajtów, więc ona również będzie miała 16 bajtów.

### Kod procedury w asemblerze
```=
proc:
    movq 8(%rdi),%rax
    movq (%rax),%rdx
    movq (%rdx),%rdx
    subq 8(%rax),%rdx
    movq %rdx,(%rdi)
    ret
```
Jako że używamy wartości `%rdi` jako wskaźnika oraz mamy prawdopodobnie nie bez powodu podaną definicję unii, to załóżmy że jako argument dostajemy wskaźnik na taką unię (`union elem* up`).
```=
proc:
    movq 8(%rdi),%rax  
    # skoro używamy niżej rax jako wskaźnika to jest to prawdopodobnie odpowiednik
    # "union struct *next = up->e2.next", next będzie również wartością zwracaną
    movq (%rax),%rdx
    # wyciągamy pierwszy element z nexta, będzie on niżej używany jako wskaźnik,
    # więc zapewne instrukcja ta odpowiada "next->e1.p"
    movq (%rdx),%rdx
    # "long p_value = *(next->e1.p)
    subq 8(%rax),%rdx
    movq %rdx,(%rdi)
    # zapisujemy pod pierwszy argument elementu podanego jako argument
    # p_value - next ->e1.y
    ret
```

### Kod w C

```c=
union elem* proc(union elem *u){
    union elem *next = u -> e2.next;
    long p_value = *(next -> e1.p);
    u -> e2.x = p_value - next -> e1.y;
    return next;
}
```

## Zadanie 5

### Definicja structów

```c=
typedef struct A {
    long u[2];
    long *v;
} SA;

typedef struct B {
    long p[2];
    long q;
} SB;
```

### Kod asemblera procedur

Wiemy, że `eval` ma sygnaturę `SB eval(SA s)`, ze względu na rozmiar przyjmowanych i zwracanych structów, będą one przekazywane przez pamięć. Zgodnie z *System V ABI* W przypadku argumentu będzie on umieszczony na stosie, a w przypadku wartości zwracanej, miejsce na nią zostanie zarezerowowane przez funkcję wołającą, a następnie wskaźnik na nią zostanie przekazany jako pierwszy, ukryty, argument.

Procedura `wrap` z kolei ma sygnaturę `long wrap(long x, long y, long z)`, czyli jej argumenty dotrą w rejestrach `RDI`, `RSI` i `RDX`.


#### Procedura wrap

```=
wrap:
    subq $72, %rsp       # rezerwujemy 72 bajty na stosie
    movq %rdx, (%rsp)    # na spodzie kładziemy 'z'
    movq %rsp, %rdx      # kopiujemy %rsp, czyli wskaźnik na z
    leaq 8(%rsp), %rax   # obliczamy adres komórki rsp + 8
    pushq %rdx           # odkładamy wartość %rdx (&z) na stos  
    pushq %rsi           # odkładamy 'y' na stos
    pushq %rdi           # odkładamy 'x' na stos
    movq %rax, %rdi      
    call eval
```
##### Zawartość rekordu aktywacji w momencie wołania eval

| Offset w wrap | Offset w eval | Zawartość |    Kontekst struktury    |
|:-------------:|:-------------:|:---------:|:------------------------:|
|      88       |      96       |           |                          |
|      80       |      88       |           |                          |
|      72       |      80       |           |                          |
|      64       |      72       |           |                          |
|      56       |      64       |           |                          |
|      48       |      56       |           |            q             |
|      40       |      48       |           |           p[1]           |
|      32       |      40       |           | %rdi, początek SB --p[0] |
|      24       |      32       |     z     |                          |
|      16       |      24       |    &z     |            v             |
|       8       |      16       |     y     |           u[1]           |
|       0       |       8       |     x     |   początek SA -- u[0]    |

##### Kontunuacja asemblera

```=
    movq 40(%rsp), %rax     # rax = sb.p[1]
    addq 32(%rsp), %rax     # rax = sb.p[1] + sb.p[0]
    imulq 48(%rsp), %rax    # sb.q * (sb.p[1] + sb.p[0])
    addq $96, %rsp          # czyścimy stos
    ret
```

##### Kod w C

```c=
long wrap(long x, long y, long z) {
    SA sa = {z,y,&z};
    SB sb = eval(sa);
    return = sb.q * (sb.p[1] + sb.p[0]);
}
```

#### Procedura eval

```=
eval:
    movq %rdi, %rax      # wskaźnik na wartość zwracaną musimy zwrócić w rax
    movq 16(%rsp), %rcx  # rcx = s.u[1]
    movq 24(%rsp), %rdx  # rdx = s.v
    movq (%rdx), %rsi    # rsi = *s.v
    movq %rcx, %rdx      # rdx = s.u[1]
    imulq %rsi, %rdx     # rdx = s.u[1] * (*s.v)
    movq %rdx, (%rdi)    # sb.p[0] = s.u[1] * (*s.v)
    movq 8(%rsp), %rdx   # rdx = s.u[0]
    movq %rdx, %rdi      # rdi = s.u[0]
    subq %rsi, %rdi      # rdi = s.u[0] - (*s.v)
    movq %rdi, 8(%rax)   # sb.p[1] = s.u[0] - (*s.v)
    subq %rcx, %rdx      # rdx = s.u[0] - s.u[1]
    movq %rdx, 16(%rax)  # sb.q = s.u[0] - s.u[1]
    ret	
```

##### Kod w C

```c=
SB eval(SA s) {
    SB sb;
    sb.p[0] = s.u[1] * (*s.v);
    sb.p[1] = s.u[0] - (*s.v);
    sb.q = s.u[0] - s.u[1];
    return sb;
}
```

## Zadanie 6

Mamy daną procedurę o sygnaturze `float puzzle6(struct P *, float)` i mamy wyzanczyć definicję `struct P`.

Zgodnie z *System V ABI* pierwszy argument znajdzie się w rejestrze `RDI`, drugi w `xmm0`. Wynik zwracamy przez `xmm0`.

```=
puzzle6:
        movq (%rdi), %rdx           # wyciągamy pierwszy element structa 
        leaq 8(%rdi), %rcx          # obliczamy wskaźnik na drugi
        xorl %eax, %eax             # zerujemy RAX (od teraz i)
        vxorps %xmm1, %xmm1, %xmm1  # zerujemy xmm1
        vmovss .LC1(%rip), %xmm2    # ładujemy 1 do xmm
.L2:    cmpq %rdx, %rax
        jge .L5                     # jeśli rax większy to skaczemy na koniec
        vfmadd231ss (%rcx,%rax,4), %xmm2, %xmm1 *
        incq %rax                   # zwiększamy i
        vmulss %xmm0, %xmm2, %xmm2 **
        jmp .L2                     # skaczemy do sprawdzenia warunku
        .L5: vmovaps %xmm1, %xmm0   # zwracamy xmm1
        ret
	
.LC1:   .long 0x3f800000 # 0 01111111(127_10) 00000...., czyli to 1

* vfmadd231ss xmm1, xmm2, xmm3, to na intelu xmm1 += xmm2 * xmm3 (tam te
cyferki mają sens) więc u nas będzie to xmm3 += xmm1 * xmm2

** vmulss %xmm0, %xmm2, %xmm2 znaczy %xmm2 = %xmm2 * %xmm0
```

### Kod w C

Z powyższego kodu możemy wywnioskować, że struct ma jakieś pole określające ile iteracji należy wykonać oraz jakąś tablicę floatów, po której będziemy się iterować.

```c=
typedef struct {
    long n;
    float x[];
}

float puzzle6(struct P *sp, float q){
    long n = sp->n;
    float sum = 0;
    float power = 1;
    
    for(int i=0; i<=n; i++){
        sum += (sp -> x[i]) * power;
        power *= q;
    }
    
    return sum;
}
```
Powyższy kod liczy sumę

$$
    \sum_{k=0}^n x_k \cdot q^k 
$$

gdzie tablica `a` i liczba `n` przekazywane są przez structa, a `q` jest drugim argumentem wywołania funkcji.

## Zadanie 7

```c=
struct Base {
    Base(int n) : data(n) {}
    int data;
    virtual int doit(int n) { return n - data; }
};

struct Derived : Base {
    Derived(int n) : Base(n + 1) {}
    int doit(int n) { return n * data; }
};

int doit(Base *bp) {
    return bp->doit(1);
}

int main(int argc, char *argv[]) {
    Base b = Base(10);
    Derived d = Derived(20);
    return doit(&b) + doit(&d);
}
```

Tak jak wspomniane jest w treści zadania, w 12 lini nie jesteśmy w stanie w czasie kompilacji powiedzieć, z której definicji funkcji `doit` będziemy musieli skorzystać, gdyż może być to zarówno ta pochodząca z `Base` jak i z `Derived`, w zależności od obiektu, który przyjdzie jako argument.

W języku C++ problem ten jest rozwiązywany przez tworzenie **tablic metod wirtualnych**.

### Tablica metod wirtualnych

#### Definicja

Tablicę metod wirtualnych, współdzielą obiekty tej samej klasy. Zawiera ona adresy metod wirtualnych, które mają zostać wywołane w przypadku obiektu danej klasy.

W wygenerowanym kodzie możemy znaleźć miejsce definicji tabeli metod wirtualnych

```=
vtable for Base:
        .quad   0
        .quad   0
        .quad   Base::doit(int)
vtable for Derived:
        .quad   0
        .quad   0
        .quad   Derived::doit(int)
```

Każdy obiekt otrzyma wskaźnik na swoją tablicę metod wirtualnych jako ukryte pole, jest to widoczne w wygenerowanym kodzie konstruktora w funkcji main:

```=
main:
    pushq   %rbx
    subq    $32, %rsp
    movq    %rsp, %rdi
    
    # Base b = Base(10);
    movq    $vtable for Base+16, (%rsp)
    movl    $10, 8(%rsp)
    
    # Derived d = Derived(20);
    movl    $21, 24(%rsp)
    movq    $vtable for Derived+16, 16(%rsp)
    
    call    doit(Base*)
    ...
```

##### Zawartość rekordu aktywacji przed wywołaniem doit

| Offset |               Zawartość               |        Uwagi        |
|:------:|:-------------------------------------:|:-------------------:|
|   24   |                  21                   | 'data' wewnątrz 'd' |
|   16   | Wskaźnik na tablicę metod dla Derived |    początek 'd'     |
|   8    |                  10                   | 'data' wewnątrz 'b' |
|   0    |  Wskaźnik na tablicę metod dla Base   |    początek 'b'     |

Widać tutaj też, że w tym przypadku kompilator umieścił wskaźniki na tablice metod wirtualnych na początku structów.

#### Użycie

Tablica ta jest używana w miejscu wywołań metod wirtualnych, czyli w tym przypadku wewnątrz procedury `doit` na zewnątrz structów:

```c=
int doit(Base *bp) {
    return bp->doit(1);
}
```

W asemblerze tłumaczy się do:

```=
doit(Base*):
        movq    (%rdi), %rax     # Wyciągamy wskaźnik na tablicę metod 
        movl    $1, %esi         # drugim argumentem* będzie 1
        movq    (%rax), %rax     # wyciągamy adres metody wirtualnej
        jmp     *%rax            # skaczemy pod ten adres
        
* drugim, dlatego że pierwszym niejawnie jest wskaźnik na obiekt, z którego
wołana jest metoda.
```

### Skąd metody doit wiedzą gdzie szukać 'data'

Okazuje się, że niestatyczne metody należące do klas, niejawnie przyjmują jako pierwszy argument wskaźnik na obiekt, z którego wołana jest ta metoda. Mają dzięki temu dostęp do jego pól. Widać to w wygenerowanym kodzie asemblera. 

#### int Base:doit(int n)
```=
Base::doit(int):
    movl    %esi, %eax     # zapisujemy 'n' do %eax
    subl    8(%rdi), %eax  # odejmujemy 'data' od 'n'
    ret
```

#### int Base:doit(int n)
```=
Derived::doit(int):
    movl    8(%rdi), %eax  # zapisujemy 'n' do %eax
    imull   %esi, %eax     # mnożymy przez 'data'
    ret
```
# ASK -- Lista 4

###### tags: `ASK`



## Zadanie 1

Mamy obliczyć wartości **operandów źródłowych** operacji `movq` na podstawie podanej tabelki.

1. %rax -- 0x100 -- zwykłe pobranie zawartości rejestru
2. 0x110 -- 0x13 -- pobranie zawartości pamięci spod tego adresu
3. $0x108 -- 0x108 -- podajemy stałą
4. (%rax) -- 0xFF -- wykonujemy dereferencję

Ogólna zasada na kolejne przypadki: `D(Rb,Ri,S) = Mem[Reg[Rb] + S*Reg[Ri] + D]`, Jeśli jakaś wartość jest opuszczona, to jest równa 0.

6. 8(%rax) -- (0x100 + 0x8) = (0x108) =  0xAB
7. 21(%rax, %rdx) -- (0x100 + 0x3 + 0x15) = (0x118) = 0x11
8. 0xFC(,%rcx,4) -- (4 * 1 + 0xFC) = (0x4 + 0xFC) = (0x100) = 0xFF
9. (%rax, %rdx, 8) -- (0x100 + 8 * 3) = (0x100 + 0x18) = (0x118) = 0x11
10. 265($rcx,%rdx,2) -- (1 + 2 * 3 + 265) = (272) = (0x110) = 0x13

## Zadanie 2

Należy podać miejsce w pamięci, w którym zostanie umieszczony wynik działania i jej wartość, dla danych podanych w tabelce.

| Instrukcja                | Miejsce              | Wartość                                                         | Uwagi                                                                                            |
| ------------------------- | -------------------- | --------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| addq %rcx, (%rax)         | 0x100                | 0x100                                                           |                                                                                                  |
| subq 16(%rax), %rdx       | RDX                  | 0x3 - (0x100 + 0x10)  = 0x3 - 0x13 = -0x10                      |                                                                                                  |
| shrq $4, %rax             | RAX                  | 0x100 >> 4 = 0x10                                               | przesunięcie logiczne w prawo                                                                    |
| incq 16(%rax)             | 0x100 + 0x10 = 0x110 | 0x13 + 1 = 0x14                                                 | inkrementacja                                                                                    |
| decq %rcx                 | RCX                  | 1 - 1 = 0x0                                                     | dekrementacja                                                                                    |
| imul 8(%rax)              | RDX:RAX              | (0x100 + 0x8) * 0x100 = (0x108) * 0x100 = 0xAB * 0x100 = 0xAB00 | Zgodnie z dokumentacją, jako drugiego operandu używamy RAX, wynik zapisujemy na dwóch rejestrach |
| leaq 7(%rcx,%rcx,8), %rdx | RDX                  | 1 + 8 * 1 + 7 = 0x10                                            |                                                                                                  |
| leaq 0xA(,%rdx,4), %rdx   | RDX                  | 4 * 3 + 0xA = 12 + 0xA = 0x16                                   |                                                                                                  |
## Zadanie 4

RDI i RSI to odpowiednio argumenty `long x` i `long y`. Prześledźmy działanie kolejnych instrukcji

* `leaq (%rdi, %rsi), %rax` -- sumuje `x` i `y` wynik zapisuje w rejestrze RAX.
* `xorq %rax, %rdi` -- wykonuje xor na wyniku poprzdniego dodawania i argumencie `x`, wynik zapisuje w miejscu argumentu `x`. Czyli teraz `x = (x+y) ^ x`.
* `xorq %rax, %rsi` -- wykonuje xor na wyniku dodawania i argumencie `y`, wynik zapisuje w miejscu argumentu `y`. Czyli teraz `y = (x+y) ^ y`.
* `movq %rdi, %rax` -- przenosi wartość rejestru RDI, czyli `(x+y) ^ x` do rejestru RAX. (RAX to rejestr przez który zwracamy wartość).
* `andq %rsi, %rax` -- wykonuje and na wyniku drugiego xora i wartości przechowywanej w RAX, czyli obecnie wyniku pierwszego xora. Zatem wartość w RAX, to teraz `((x + y) ^ y) & ((x + y) ^ x)`
* `shrq $63, %rax` -- wykonuje przesunięcie logiczne bitów w prawo. Czyli zawartość RAXa to teraz `(((x + y) ^ y) & ((x + y) ^ x)) >> 63` 
* `ret` -- zwracamy zawartość rejestru RAX.

### Funkcja w języku C

```c=
long decode(long x, long y) {
	return (unsigned long)( ((x + y) ^ y) & ((x + y) ^ x) ) >> 63;
}
```

Ciekawostka: Jest to funkcja sprawdzająca czy nastąpił nadmiar lub niedomiar w dodawaniu x'a i y'ka.

#### Bardziej zwięzła forma

```c=
long decode(long x, long y) {
	return (unsigned long)(~(x ^ y) & (x + y ^ x)) >> 63;
}
```

Czyli wcześniej się pytaliśmy czy najstarszy bit x'a i y'ka są róźnie od wyniku dodawania, pytamy się czy najstarsze bity są takie same, i któryś z nich jest różny od wyniku dodawania.

## Zadanie 5

### Przypomnienie z Listy 0

Kod w C, który dokonuje takiej konwersji ma postać
```c=
x = (x >> 16) | (x << 16);
x = ((x >> 8) & 0x00FF00FF) | ((x & 0x00FF00FF) << 8);
```
Czyli zamieniamy połówki miejscami, a następnie połówki połówek, czyli bajty.

Operacje podobne do `(x >> 16) | (x << 16)`, możemy rozumieć jako cykliczne przesunięcia w prawo (w tym przypadku o 16). Zauważmy jednak że nie mamy bezpośredniego  dostępu do starszych bajtów liczby zapisanej w rejestrze, więc jeśli chcemy zminimalizować liczbę operacji w assemblerze, musimy zrobić to w sprytniejszej kolejności.

### Zapis w assemblerze

Argument zapisany jest w rejestrze EDI, wynik będziemy zwracać przez EAX.

`ABCD -> ABDC -> DCAB -> DCBA`

Najpierw zamieniamy dolną ćwiartkę, później połówki i znowu dolną ćwiartkę.

```whatever
convert: movl %edi, %eax
         rolw $8, %ax
         roll $16, %eax
         rolw $8, %ax
         ret
```

### Wyrażenie tłumaczące się do rora/rola

Przykładem może być wyżej wymienione `(x >> 16) | (x << 16)` dla zmiennej `x` typu `uint32_t`.

## Zadanie 6

Musimy zaimplementować dodawanie liczb 128 bitowych, które są podane "w dwóch częściach".

* RDI:RSI -- argument `x`
* RDX:RCX -- argument `y`

Zwracać wynik będziemy przez rejestry RDX i RAX.

Oczywistym podejściem jest dodanie najpierw młodszej partii bitów, później starszej z uwzględnieniem ewentualnego przeniesienia. Pomocna będzie w tym instrukcja `adc`, która uwzględnia w obliczeniach flagę przeniesienia (CF), którą ustawić może instrukcja `add`.

```whatever=
add128: addq %rsi, %rcx
        adcq %rdi, %rdx
        movq %rcx, %rax
        ret
```

## Zadanie 7

Musimy zaimplementować dodawanie liczb 128 bitowych, które są podane "w dwóch częściach".

* RDI:RSI -- argument $x$
* RDX:RCX -- argument $y$

Dokładniej

* RDI -- argument $x_{[127:64]}$
* RSI -- argument $x_{[63:0]}$
* RDX -- argument $y_{[127:64]}$
* RCX -- argument $y_{[63:0]}$

Zwracać wynik będziemy przez rejestry RDX i RAX.

Skorzystamy z własności podanej w treści zadania tj. że

$$
n = n_{[127:64]}\cdot 2^{64} + n_{[63:0]}
$$

Zapiszmy więc
$$
x = x_{[127:64]}\cdot 2^{64} + x_{[63:0]} \\
y = y_{[127:64]}\cdot 2^{64} + y_{[63:0]}
$$
Po wymnożeniu
$$
x \cdot y =x_{[127:64]}y_{[127:64]} \cdot 2^{128} + (x_{[127:64]}y_{[63:0]} + y_{[127:64]}x_{[63:0]}) \cdot 2^{64} + x_{[63:0]}y_{[63:0]}
$$

Nasz zwracany wynik będzie miał tylko 128 bitów, możemy więc opuścić obliczanie pierwszego wyrazu sumy. Zostaje

$$
(x_{[127:64]}y_{[63:0]} + y_{[127:64]}x_{[63:0]}) \cdot 2^{64} + x_{[63:0]}y_{[63:0]}
$$

Dla lepszego wyobrażenia, to samo wyrażenie z nazwami rejestrów.

$$
(\%rdi\cdot \% rcx + \%rdx \cdot \%rsi) \cdot 2^{64} + \%rsi \cdot \%rcx
$$

Można zauważyć też od razu że górne 64 bity pierwszego wyrazu nie zostaną włączone do wyniku, gdyż znajdą się na pozycjach [191:128]. Dolne natomiast mogą zostać zsumowane z górnymi iloczynu w drugim wyrazie sumy (który jest 128 bitowy). Pamiętamy też, że operacja `mul` zapisuje swój wynik między innymi na rejestrze RDX, w którym znajduje się część naszej zmiennej. Ta część zmiennej występuje tylko raz w obliczeniach, wystarczy więc wykonać to działanie jako pierwsze i unikniemy niepotrzebnego kopiowania jej wartości.

```whatever=
mult128: movq %rsi, %rax
         mulq %rdx
         movq %rax, $r8
         movq %rdi, %rax
         mulq %rcx
         addq %rax, %r8
         movq %rsi, %rax
         mulq %rcx
         addq %r8, %rdx
         ret
```

## Zadanie 8

Argumenty otrzymujemy w dwóch rejestrach.

* RDI -- argument `x`
* RSI -- argument `y`

### Kod ze skokiem warunkowym

Możemy wykorzystać instrukcję `jae`, która wykonuje skok, jeśli flaga CF jest ustawiona na 0. Zostanie ona ustawiona jeśli wynik dodawania `x` i `y` nie zmieści się w rejestrze.

```wtv=
addu:   addq %rdi, %rsi
        jae  return
        movq $0xFFFFFFFFFFFFFFFF, %rsi
return: movq %rsi, %rax
        ret
```

### Kod bez skoku

Instrukcja `sbb`, wykonuje to samo co `sub`, tylko bierze pod uwagę ewentualną pożyczkę, czyli wartość flagi CF, i dodaje ją do operandu źródłowego (równoważne z odjęciem, czyli pożyczeniem, od docelowego). Możemy zatem wykorzystać ją do zczytania wartości flagi i uzyskania wartości -1 (która ma same jedynki), lub 0. Następnie wykonując bitowy OR z wynikiem dodawania, albo nie zmienimy go, jeśli CF był 0, lub zmienimy na same jedynki (czyli również ULONG_MAX), jeśli CF był 0.

```whatever=
addu: addq %rdi, %rsi
      sbbq %rax, %rax
      orq %rsi, %rax
```

## Zadanie 9

Zgodnie ze wskazówką użyjemy instrukcji `adc`, `sbb` i `neg`. Ta ostatnia, zgodnie z dokumentacją ma ciekawą własność, tj. ustawia flagę CF na 0 jeśli wartość operandu była zerem, a na 1 w przeciwnym przypadku. Może zatem być użyta jako test na obecność zera.

### Kod

Załóżmy, że argumenty `x` i `y`, otrzymujemy w rejestrach RDI i RSI, a wynik zwracamy przez RAX.

```whatever=
cmp: subq %rsi, %rdi
     sbbq %rax, %rax
     negq %rdi
     adcq %rax, %rax
     ret
```

### Objaśnienie

* `subq %rsi, %rdi` -- Wykonujemy odejmowanie x - y, jeśli x < y, to flaga pożyczki (CF) zostanie ustawiona na 1.
* `sbbq %rax, %rax` -- Jeśli flaga pożyczki była ostawiona, tj. jeśli x < y, to w rejestrze RAX znajdzie się -1, w przeciwnym przypadku 0.
* `negq %rdi` -- Chcemy jeszcze sprawdzić czy x był równy y, w tym celu sprawdzamy czy wynik odejmowania x - y był zerem. Wtedy `neg` ustawi flagę CF na 0, lub na 1 w przeciwnym przypadku.
* `adcq %rax, %rax` -- Jeśli x < y to w RAX'ie się znajduje -1, a CF=1, bo x-y był niezerowy, czyli w RAX'ie zostanie zapisana wartość -1 + -1 + 1, czyli -1. Jeśli x = y, to w RAX'ie znajduje się 0 i flaga CF to również zero, dostaniemy 0 + 0 + 0, czyli 0. Jeśli x > y, to w RAX'ie się znajduje 0, ale flaga CF to 1, więc mamy 0 + 0 + 1, czyli 1.
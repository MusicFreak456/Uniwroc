# ASK -- Lista 9

###### tags: `ASK`

## Zadanie 1

### Miejsce występowania odwołań do symboli

Kompilujemy do pliku relokowalnego `relo3.o` i badamy wynik polecenia `objdump -D -r`

```
Disassembly of section .text:

0000000000000000 <relo3>:
   0:	8d 47 9c             	lea    -0x64(%rdi),%eax
   3:	83 f8 05             	cmp    $0x5,%eax
   6:	77 1e                	ja     26 <relo3+0x26>
   8:	89 c0                	mov    %eax,%eax
   
   a:	48 8d 15 00 00 00 00 	lea    0x0(%rip),%rdx        # 11 <relo3+0x11>
			d: R_X86_64_PC32	.rodata-0x4
			
                              # ^
                              # |
                              # Tutaj jest odwołanie
			
  11:	48 63 04 82          	movslq (%rdx,%rax,4),%rax
  15:	48 01 d0             	add    %rdx,%rax
  [...]
  
  
Disassembly of section .rodata:

0000000000000000 <.rodata>:
	...
	# Odwołania są w każdym wierszu poniżej
	0: R_X86_64_PC32	.text+0x2a
	4: R_X86_64_PC32	.text+0x1e
	8: R_X86_64_PC32	.text+0x2e
	c: R_X86_64_PC32	.text+0x2a
	10: R_X86_64_PC32	.text+0x2e
	14: R_X86_64_PC32	.text+0x36
```

### Zawartość tablic relokacji

```
Relocation section '.rela.text' at offset 0x408 contains 1 entry:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
00000000000d  000500000002 R_X86_64_PC32     0000000000000000 .rodata - 4

Relocation section '.rela.rodata' at offset 0x420 contains 6 entries:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
000000000000  000200000002 R_X86_64_PC32     0000000000000000 .text + 2a
000000000004  000200000002 R_X86_64_PC32     0000000000000000 .text + 1e
000000000008  000200000002 R_X86_64_PC32     0000000000000000 .text + 2e
00000000000c  000200000002 R_X86_64_PC32     0000000000000000 .text + 2a
000000000010  000200000002 R_X86_64_PC32     0000000000000000 .text + 2e
000000000014  000200000002 R_X86_64_PC32     0000000000000000 .text + 36
```

### Obliczanie wartości do wstawienia

Zakładamy zgodnie z treścią, że `relo3` zostanie umieszczone pod adresem `0x1000`, a tablica skoków pod `0x2000`.

#### Procedura relo3

`d: R_X86_64_PC32	.rodata-0x4`

Miejsce z którego podmieniamy to `0x100d`, tablica skoków jest teraz pod adresem `0x2000`, addend wynosi -4, zatem liczymy

```
*refptr = (unsigned) (ADDR(r.symbol) + r.addend - refaddr) = (unsignend) (0x2000 - 0x4 - 0x100d) = FEF
```

#### Tablica skoków

Będziemy je liczyć analogicznie do powyższego, pamiętając że tablica skoków znajduje się teraz pod adresem `0x2000`.

`0: R_X86_64_PC32	.text+0x2a`
```
*refptr = (unsigned) (ADDR(r.symbol) + r.addend - refaddr) 
= (unsignend) (0x1000 + 0x2a - 0x2000) = FFFF F02A
```

`4: R_X86_64_PC32	.text+0x1e`

```
*refptr = (unsigned) (ADDR(r.symbol) + r.addend - refaddr) 
= (unsignend) (0x1000 + 0x1e - 0x2004) = FFFF F01A
```

`8: R_X86_64_PC32	.text+0x2e`

```
*refptr = (unsigned) (ADDR(r.symbol) + r.addend - refaddr) 
= (unsignend) (0x1000 + 0x2e - 0x2008) = FFFF F026
```

`c: R_X86_64_PC32	.text+0x2a`

```
*refptr = (unsigned) (ADDR(r.symbol) + r.addend - refaddr) 
= (unsignend) (0x1000 + 0x2a - 0x200c) = FFFF F01E
```

`10: R_X86_64_PC32	.text+0x2e`

```
*refptr = (unsigned) (ADDR(r.symbol) + r.addend - refaddr) 
= (unsignend) (0x1000 + 0x2e - 0x2010) = FFFF F01E
```

`14: R_X86_64_PC32	.text+0x36`

```
*refptr = (unsigned) (ADDR(r.symbol) + r.addend - refaddr) 
= (unsignend) (0x1000 + 0x36 - 0x2014) = FFFF F022
```

## Zadanie 2

Dekorowaniu podlegają wszystkie symbole widoczne z zewnątrz modułu, z wyjątkiem zmiennych w globalnej przestrzeni nazw i symboli odwołujących się do języka C.

Na etapie konsolidacji stracimy informacje o przestrzeniach nazw lub liczbie i typie argumentów i np. nie będziemy wstanie odróżnić funkcji od jej przeciążenia. W tym celu tego typu informacje wkodowywane są w ich nazwy.

Proces ten można odwrócić za pomocą narzędzia `c++filt`.

W każdym przypadku prefiks `_Z` oznacza, że nazwa została udekorowana.

### Pierwszy przykład
`_Z4funcPKcRi` -> `func(char const*, int&)`

`4func` - nazwa identyfikatora, 4 na początku wyznacza jego długość. Utworzona wg. reguły 
`<source-name> ::= <*positive length* number> <identifier>`

`PKc Ri` - zakodowana sygnatura funkcji wg. reguły 
`<bare-function-type> ::= <*signature* type>+`

Gdzie `type` to:
```
<type> ::= <builtin-type>
       ::= <qualified-type>
       [...]
       ::= P <type>        # pointer
       ::= R <type>        # l-value reference
       ::= O <type>        # r-value reference (C++11)
       ::= C <type>        # complex pair (C99)
       ::= G <type>        # imaginary (C99)
       ::= <substitution>  # See Compression below
	   
<qualified-type>     ::= <qualifiers> <type>
<qualifiers>         ::= <extended-qualifier>* <CV-qualifiers>
[...]
<CV-qualifiers>      ::= [r] [V] [K] 	  # restrict (C99), volatile, const
<ref-qualifier>      ::= R              # & ref-qualifier
<ref-qualifier>      ::= O              # && ref-qualifier


<builtin-type> ::= v	# void
               ::= w	# wchar_t
               ::= b	# bool
               ::= c	# char
               ::= a	# signed char
               ::= h	# unsigned char
               ::= s	# short
               ::= t	# unsigned short
               ::= i	# int
               ::= j	# unsigned int
               ::= l	# long
               ::= m	# unsigned long
               ::= x	# long long, __int64
               ::= y	# unsigned long long, __int64
               ::= n	# __int128
               ::= o	# unsigned __int128
               ::= f	# float
               ::= d	# double
               itd....
```
Zatem `PKc` oznacza stały wskaźnik na chara, a `Ri` oznacza referencję do inta.

### Drugi przykład

`_ZN3Bar3bazEPc` -> `Bar::baz(char*)`

`N3Bar3bazE` -- Litera `N` na początku i `E` na końcu oznacza że jest to obiekt zadeklarowany wewnątrz przestrzeni nazw, albo w zasięgu klasy.

```
<nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
              [...]
			  
<unqualified-name> ::= <operator-name> [<abi-tags>]
                   ::= <ctor-dtor-name>  
                   ::= <source-name>
                   [...]
			  
<prefix> ::= <unqualified-name>                 # global class or namespace
         ::= <prefix> <unqualified-name>        # nested class or namespace
         [...]
```

Zatem `3Bar` będzie nazwą klasy, lub przestrzeni nazw, a `3baz` -- obiektu, który rozpatrujemy. Są to nazwy utworzone wg. wspomnianych już wcześniej reguł.

Podobnie `Pc` to zakodowana sygnatura funkcji. Tym razem przyjmuje ona wskaźnik na chara.

### Trzeci przykład

`_ZN3BarC1ERKS_` -> `Bar::Bar(Bar const&)`

`N3BarC1E` -- ta część jest podobna jak w poprzednim podpunkcie. `3Bar` oznacza nazwę klasy, z której pochodzi rozpatrywany obiekt, ale zamiast jego nazwy, mamy użyte specjalne oznaczenie oznaczające konstruktor.

```
<ctor-dtor-name> ::= C1			# complete object constructor
                 ::= C2			# base object constructor
                 ::= C3			# complete object allocating constructor
                 ::= CI1 <base class type>	# complete object inheriting constructor
                 ::= CI2 <base class type>	# base object inheriting constructor
                 ::= D0			# deleting destructor
                 ::= D1			# complete object destructor
                 ::= D2			# base object destructor
```

Następnie zakodowana jest sygnatura konstruktora `RKS_`, `R` i `K` oznaczają to co poprzednio. `S_` oznacza podstawienie pierwszego "podstawialnego" komponentu patrząc od lewej do prawej. Nie liczą się do nich np. typy wbudowane. Mechanizm ten jest stosowany dla skrócenia zapisu. W naszym przypadku pierwszym podstawialnym komponentem od lewej w całym ciągu jest 'Bar' (chodzi o ich faktyczną postać, nie ciąg znaków jakimi są reprezentowane), zatem to on zostanie podstawiony w miejsce `S_`.

### Czwarty przykład

`_ZN3foo6strlenER6string` -> `foo::strlen(string&)`

`N3foo6strlenE` --  tak samo jak poprzednio, `3foo` koduje przestrzeń nazw, lub klasę, `6strlen` nazwę rozpatrywanego obiektu.

`R6string` -- koduje sygnaturę funkcji. `R ` oznacza to co zwykle. `6string` oznacza typ o nazwie `string`.

## Zadanie 3

### Lista kroków dla rozwiązania zadania

* Skompiluj program poleceniem `make ropex`.
* Zdeasembluj program poleceniem `objdump -D ropex`.
* Wyszukaj procedurę `gadget` i zapisz jej adres. Np. `0x401d67`.
* Wyszukaj dowolne wystąpienie procedury `syscall` i zapisz jego adres. Np. `0x40232d`.
* W pliku `ropex.in.txt` podmień pierwsze wystąpienie `baad f00d` na pozycji `38`, na adres procedury `gadget` zapisany w *little endian*, czyli w tym przypadku `671d 4000`,
* Drugie wystąpienie `baad f00d` analogicznie zamieniamy na adres instrukcji `syscall`. `baad f00d` -> `2d23 4000`.
* Konwertujemy plik do postaci binarnej poleceniem `make ropex.in`.
* Po uruchomieniu programu `ropex` z argumentem `ropex.in` uruchomi się program `nyancat` (jeśli jest zainstalowany).

### Reszta rozwiązania w trakcie prezentacji

## Zadanie 4

### Lista kroków dla rozwiązania zadania

* Kompilujemy program ze zmienioną flagą z `-fno-stack-protector` na `-fstack-protector`
* Powtarzamy kroki z poprzedniego zadania.
* Uruchomienia programu zakończy się błędem `stack smashing detected`, który oznacza, że ustawiony kanarek został uszkodzony.
* Kanarek zostanie pobrany instrukcją `mov %fs:(%rbx), %rax`. Odczytajmy wartość w `%rax` za pomocą gdb. Przykładowe wartości:
	* Wartość za pierwszym razem: `0xbee3ddf9c007f100`.
	* Wartośc kanarka za drugim razem: `0x9860f44f3722ca00`.
* Zmieniamy flagi po raz kolejny. Usuwamy `-static` i zamieniamy `fno-pie` na `-fpie`.
* Adres procedury `gadget` możemy podejrzeć poleceniem `info address gadget` w gdb, musimy jednak najpierw wyłączyć automatyczne wyłączanie ASRL w gdb, poleceniem `set disable-randomization off` przed uruchomieniem programu.
	* Adres za pierwszym razem: `0x55cc68929224`.
	* Adres za drugim razem: `0x55f23ad6c224`.
* Na koniec dodajemy opcję kompilacji `-z noexecstack` i uruchamiamy `ropex`. Żeby skorzystać z `pmap` musimy poznać id procesu, możemy to zrobić poleceniem `ps a`. Wydruk programu pmap dla procesu programu `ropex`:
```
[...]
00007f9ef762b000      4K rw--- ld-2.31.so
00007f9ef762c000      4K rw---   [ anon ]
00007ffebde8e000    132K rw---   [ stos ]
00007ffebdf9c000     12K r----   [ anon ]
[...]
```
Jak widać stos nie jest teraz wykonywalny.

## Zadanie 5

Zgodnie z sugestią możemy podglądać to co wyprodukuje `gcc` dla zadanego kodu. Kod będzie generowany za pomocą polecenia `gcc -c -fno-asynchronous-unwind-tables -S`

### Definicja globalnej funkcji foobar

#### Kod wejściowy

```c=
int foobar(){
    return 0;
}
```

#### Kod wyjściowy

```=
	.file	"test.c"
	.text
	.globl	foobar
	.type	foobar, @function
foobar:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	popq	%rbp
	ret
	.size	foobar, .-foobar
	.ident	"GCC: (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0"
	.section	.note.GNU-stack,"",@progbits
```

#### Użyte dyrektywy

`.file` -- rozpoczyna nowy plik
`.text` -- informuje że poniższy kod ma być umieszczony w sekcji `.text`.
`.globl` -- definiuje symbol, który będzie widoczny na zewnątrz pliku
`.type` -- ustawia typ symbolu
`.size` -- ustawia rozmiar symbolu, w tym przypadku będzie on obliczany w locie, odejmując od licznika adresu początek funkcji.

`.ident` -- używana do umieszczania tagów w plikach relokowalnych, zazwyczaj umieszczane są one w sekcji `.comment`.

### Definicja lokalnej struktury

#### Kod wejściowy

```c=
static const struct {
    char a[3]; int b; long c; float pi;
} baz = { "abc", 42, -3, 1.4142 };
```

#### Kod wyjściowy

```=
	.file	"test.c"
	.text
	.section	.rodata
	.align 16
	.type	baz, @object
	.size	baz, 24
baz:
	.ascii	"abc"
	.zero	1
	.long	42
	.quad	-3
	.long	1068827777
	.zero	4
	.ident	"GCC: (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0"
	.section	.note.GNU-stack,"",@progbits
```

#### Nowe użyte dyrektywy

`.align` -- przesuwa licznik adresu na liczbę podzielną przez podany argument
`.ascii` -- umieszcza ciąg znaków w danym miejscu pamięci (bez zera na końcu)
`.zero` -- wstawia liczbę 1-bajtowych zer równą jej argumentowi, tutaj użyte do paddingu wewnątrz struktury
`.long` -- to samo co `.ascii`, tylko z longiem, w naszej strukturze użyte także do załadowania floata wartością `3,14`.
`.quad` -- to samo co `.long`, tylko dla 64-bitowych wartości.

### Rezerwowanie miejsca dla tablicy 100-elementowej

#### Kod wejściowy

```c=
long array[100];
```

#### Kod wyjściowy

```=
	.file	"test.c"
	.text
	.comm	array,800,32
	.ident	"GCC: (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0"
	.section	.note.GNU-stack,"",@progbits
```

#### Nowe użyte dyrektywy

`.comm` -- deklaruje symbol o nazwie takiej jak pierwszy argument w sekcji `common` o rozmiarze zadanym drugim argumentem i wyrównaniu przekazywanym jako trzeci, opcjonalny, argument. Stosowany kiedy zmienne nie są inicjowane, więc wystarczy zapamiętać ich rozmiar.

### Wynik deklaracji w tablicy symboli


```
Symbol table '.symtab' contains 11 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND
     1: 0000000000000000     0 FILE    LOCAL  DEFAULT  ABS test.c
     2: 0000000000000000     0 SECTION LOCAL  DEFAULT    1
     3: 0000000000000000     0 SECTION LOCAL  DEFAULT    2
     4: 0000000000000000     0 SECTION LOCAL  DEFAULT    3
     5: 0000000000000000     0 SECTION LOCAL  DEFAULT    4
     6: 0000000000000000    24 OBJECT  LOCAL  DEFAULT    4 baz
     7: 0000000000000000     0 SECTION LOCAL  DEFAULT    6
     8: 0000000000000000     0 SECTION LOCAL  DEFAULT    5
     9: 0000000000000020   800 OBJECT  GLOBAL DEFAULT  COM array
    10: 0000000000000000    11 FUNC    GLOBAL DEFAULT    1 foobar
```


## Zadanie 7
 
### Czym charakteryzuje się PIC
 
Cechą charakterystyczną kodu relokowalnego jest fakt, że może on być załadowany i wykonywany pod dowolnym adresem bez potrzeby dodatkowych modyfikacji wykonywanych przez konsolidator.

### Czym jest .got

Jest sekcja zawierająca strukturę danych GOT (global offset table), umieszczana na początku degmentu `data` używana do niebezpośredniego odwoływania się do obiektów globalnych. Kompilator dla każdego takiego odwołania generuje wpis w tablicy i rekord relokacji, który zostanie rozwiązany w czasie ładowania programu. Następnie odwołanie do symbolu będzie realizowane niebezpośrednio w sposób podobny do tego:

```
call L1
L1: popl %ebx             # ebx contains the current PC
addl $VAROFF, %ebx        # ebx points to the GOT entry for var
movl (%ebx), %eax         # reference indirect through the GOT
movl (%eax), %eax
```

Dodatkowo pierwsze trzy elementy elementy GOT są specjalne

`GOT[0]` -- adres sekcji `.dynamic`
`GOT[1]` -- dodatkowe informacje opisujące plik
`GOT[2]` -- punkt wejścia do konsolidatora dynamicznego


### Lazy binding

Odwołania do procedur mogłyby być realizowane w identyczny sposób, jedna powodowałoby to zwiększenie liczby instrukcji potrzebnych do wykonania procedury przy każdym jej wywołaniu. Jest to rozwiązane zatem w sprytniejszy sposób.

Definiowana jest dodatkowa struktura PLT (procedure linkage table) i oprócz wpisów w tablicy GOT, generowane są dodatkowo wpisy w tablicy PLT.

#### Czym jest .plt

Jest to sekcja przechowująca strukturę PLT umieszaczana w segmencie `code`. Zawiera ona po jednym elemencie dla każdego odwołania do procedury z biblioteki współdzielonej, w postaci:

```
PLT[1] <nazwa>
	8048454:  jmp *0x8049680  # skok do GOT tej procedury
	804845a:  pushl $0x0      # wsadzenie identyfikatora procedury na stos
	804845f:  jmp 8048444 	   # skok do PLT[0]
```

Przy czym wpis `PLT[0]` jest specjalny i ma postać:

```
PLT[0]
	8048444:  pushl 0x8049678  # push &GOT[1]
	804844a:  jmp *0x804967c   # skok do *GOT[2], czyli konsolidatora dynamicznego
	8048450:  padding
	8048452:  padding
```
 
W wiązaniu leniwym wpisy GOT dla procedur na początkowo zawierają adresy drugiej instrukcji odpowiedniego wpisu w PLT, dlatego przy pierwszym wywołaniu zostanie uruchomiony konsolidator, który ściągnie informacje ze stosu i zmodyfikuje wpis GOT, tak żeby przy kolejnych wywołaniach skok był wykonany bezpośrednio do wywoływanej procedury.

### Dlaczego .got jest modyfikowalny

Bo zawiera dane, które chcemy móc modyfikować w trakcie działania programu, np wpisy GOT dla procedur.

### Dlaczego kod i .plt nie jest

Dla bezpieczeństwa. Obie zawierają wykonywany kod, a nie chcemy żeby program był w stanie modyfikować swój kod w trakcie działania.

### Co znajduje się w sekcji .dynamic

W tej sekcji znajdują się informacje o użwanych bibliotekach współdzielonych, które muszą zostać załadowane przed uruchomieniem programu.

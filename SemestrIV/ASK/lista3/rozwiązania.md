# ASK -- Lista 3

###### tags: `ASK`

## Zadanie 2

### Zamiana liczby

W standardzie *IEEE 754-2008* 16 bitowe liczby zminennopozycje reprezentowane są w postaci

* $s$ -- 1 bit znaku. Wartość 1 jeśli ujemna.
* $exp$ -- 5 bitów na wykładnik
* $frac$ -- 10 bitów na część ułamkową

Wartość liczby to $(-1)^s\cdot M \cdot 2^E$, gdzie $E=exp - Bias$.
Bias będzie wynosił $Bias = 2^5 - 1 = 15$.

Zapiszemy w tej postaci $1.5625 \cdot 10^{-1}$, czyli $0.152625$. Napierw zapiszemy ją w postaci binarnej.
|         |        |     |
| ------- | ------ | --- |
| 0.15625 | 0.3125 | 0   |
| 0.3125  | 0.625  | 0   |
| 0.625   | 1.25   | 1   |
| 0.25    | 0.5    | 0   |
| 0.5     | 1      | 1   |

Zatem $0.152625_{10} = 0.00101_{2}$. Musimy przesunąć przecinek w prawo o trzy miejsca w prawo, czyli $E=-3$, stąd $exp = 12_{10}=01100_{2}$. Bit znaku to $s=0_{2}$. Część ułamkowa $frac$ to $0100000000_{2}$. Zatem liczbę tą kodujemy jako
$$
0\ 01100\ 0100000000
$$

### Porównanie z float

#### 16 bitów

##### Znormalizowane

* Największe co do modułu: $0\ 11110\ 1111111111 = 6.5504 \cdot 10^{4}$
* Najmniejsze co do modułu: $0\ 00001\ 0000000000 \approx 6.1036 \cdot 10^{-5}$

#### Zdenormalizowane

* Najmniejsze: $0\ 00000\ 0000000001 = 2^{-24} \approx 5.96 \cdot 10^{-8}$

#### 32 bity

##### Znormalizowane
* Największe co do modułu: $3.4028 \cdot 10^{38}$
* Najmniejsze co do modułu: $1.1754 \cdot 10^{-38}$

##### Zdenormalizowane
* Najmniejsze: $2^{-149} \approx1.401\cdot10^{-45}$

## Zadanie 3

Najpierw zamienimy podane liczby na postać 16-bitowego floata z zadania wyżej.

* $3.984375_{10}\cdot 10^{-1}=0.3984375_{10}=0.0110011_{2}$. Przesuwamy przecinek dwa miejsca w prawo, żeby mantysa była w przedziale $[1,2)$. Czyli mamy $E=-2$ i bias $15$, zatem $exp = 13_{10}=1101_2$ bit znaku to $0$.
$$
	0\ 01101\ 1001100000
$$

* $3.4375_{10}\cdot 10^{-1}=0.34375_{10}=0.01011_{2}$. Zamieniamy analogicznie jak wyżej.
$$
	0\ 01101\ 0110000000
$$

* $1.771_{10}\cdot 10^{3}=1771_{10}=11011101011_2$. Przesuwamy przecinek 10 miejsc w lewo, żeby mantysa była w przedziale $[1,2)$. Czyli mamy $E=10$ i bias $15$, zatem $exp = 25_{10}=11001_2$ bit znaku to $0$.
$$
	0\ 11001\ 1011101011
$$

### Guard, round, sticky

**Guard** -- ostatni bit, który zmieści się na mantysie
**Round** -- pierwszy bit, który się nie zmieści
**Sticky** -- wartość operacji $OR$ na bitach młodszych od round.

### Dodawanie od lewej

Pierwsze dwie liczby są już wyrównane więc dodajemy ich mantysy.
```
 1.1001100000
+1.0110000000
-----------
10.1111100000
```
Musimy jeszcze przesunąć przecinek. Wynik pierwszego dodawania $1.011111 \cdot 2^{-1}$. Teraz dodajemy trzecią.
```
 11011101011.00000000
+          0.10111110
---------------------
 11011101011.10111110 = 11011101100
```
Bit $round$ i wartość $sticky$ to $1$, zatem zaokrąglamy w górę, resultat to $11011101011_2=1772$

### Dodawanie od prawej

Wykonując dodawanie od prawej z pierwszego dodawania otrzymalibyśmy $1771$.

```
 11011101011.000000000000
+          0.010110000000
-------------------------
 11011101011.010110000000 = 11011101011
```
Z drugiego podobnie
```
 11011101011.000000000000
+          0.010110000000
-------------------------
 11011101011.010110000000 = 11011101011
```

Czyli wynik będzie inny, równy $1771$.

## Zadanie 4

`x,y,z` -- zmienne typu `int32_t`
`dx,dy,dz` -- przekonwertowane wartości `x,y,z` na typ `double`

```c= 
(float)x == (float)dx
```
Zawsze obliczy się do **prawdy**, double poprawnie zareprezentuje dowolną zmienną typu `int32`, a jego konwersja do `float` będzie się wiązała z taką samą utratą bitów, co przy bezpośrednim castowanie z inta.
```c=
dx - dy == (double)(x-y)
```
To wyrażenie może obliczyć się do **fałszu**, ponieważ o ile double jest w stanie dobrze zareprezentować wyniki odejmowania dwóch zmiennych z zakresu zmiennych typu `int32`, to odejmowanie takich liczb może nam się przekręcić, np dla `x = INT32_MIN, y = 1`.
```c=
(dx + dy) + dz == dx + (dy + dz)
```
Dla wartości z zakresu zmiennych `int32_t` jest to **prawda**, gdyż wyniki tych działań mogą być reprezentowane dokładnie na zmiennych typu `double`.
```c=
(dx * dy) * dz == dx * (dy * dz)
```
Wyrażenie może obliczyć sie do **fałszu**, ponieważ nie jesteśmy w stanie reprezentować dokładnie wyników mnożenia dowolnych dwóch liczb z zakresu typu `int`, czyli będziemy wykonywać zaokrąglenia.

### Przykład

Weźmy wartości $x = 6, y = 2^{30} + 1, z = 2^{23} + 3$. Wartości `y` i `z` są dobrane tak, pomnożone razem powodowały zaokrąglenie w mantysie. Prześledźmy obliczenia:

#### Od lewej

```
6
x
8 388 611
=
50 331 666
binarnie:
11000000000000000000010010 <- mieści się w mantysie

50 331 666
x
1 073 741 825
= 54 043 214 906 130 450
binarnie:
11000000000000000000010010000011000000000000000000010|010
```

#### Od prawej
```
1 073 741 825
x
8 388 611
= 9 007 202 484 355 075
binarnie:
10000000000000000000001100000010000000000000000000001|1
Zaokrąglamy do parzystej:
10000000000000000000001100000010000000000000000000010

10000000000000000000001100000010000000000000000000010_2
x
110_2
=
11000000000000000000010010000011000000000000000000011|00
```
Czyli otrzymaliśmy dwie liczby o różnej mantysie, więc nie mogą być sobie równe.


```c=
dx / dx == dz / dz
```
Wyrażenie obliczy się do **fałszu**, np. dla wartości zmiennych `x = 0, y = 1`, gdyż dzielenie przez 0 w liczbach zmiennoprzecinkowych może zostać wykonane i zwraca wartość `NaN`, która jest różna od 1.


## Zadanie 5
Zapisujemy reprezentację bitową liczb zmiennoprzecinkowych $f$ i $g$ na zmiennej typu uint32.

### Zamiana znaku
```c=
x ^ 0x80000000
```
### Logarytm
Czyli wartość $exp - bias$. 

$exp$ jest zapisany na 8 kolejnych bitach po bicie znaku. Bias to 127.

```c=
((x & 0x7F800000) >> 23) - 127
```

### Równość

Przyrównujemy tak samo jak zwykłe inty, pamiętamy tylko że -0 = 0.

```c=
(x == y) | ( (x | y) == 0x80000000)
```

### Porównanie

```c=
( ((x ^ y) & x ) | (x & y & (y < x)) | 
(~(x & y) & (x < y) ) >> N-1) & ( (x|y) != 0x80000000 )
```

Gdzie `x < y` oznacza `((x & 0x7FFF) >> 1) - ((y & 0x7FFF) >> 1) - (~x & y & 1)`
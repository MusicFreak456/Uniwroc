# AiSD -- Lista0

## Zadanie 4

### Algorytm mnożenia liczb po rosyjsku

* Obliczamy ciąg $a_1, a_2, ..., a_k$ taki, że $a_1 = a, a_k=1, a_{i+1} = \lfloor \frac{a_i}{2} \rfloor$ (dla i = 1,...,k-1)
* Obliczamy ciąg $b_1, b_2, ..., b_k$ taki, że $b_1= b, b_{i+1} = 2b_i$ (dla i = 1,...,k-1)
* Obliczamy $\sum_{i=1, a_i nieparzyste}^k b_i$

### Przykład

$a=33$ 
$b=42$
| a  | b  |
| -- | -- |
| 33 | 42 |
| 16 | 84 |
| 8  | 168 |
| 4  | 336 |
| 2  | 672 |
| 1  | 1344 |

Usuwamy parzyste $a_i$.

| a  | b  |
| -- | -- |
| 33 | 42 |
| ~~16~~ | ~~84~~ |
| ~~8~~  | ~~168~~ |
| ~~4~~  | ~~336~~ |
| ~~2~~  | ~~672~~ |
| 1  | 1344 |

Dodajemy pozostałe $b_i$. $42 + 1344 = 1386 = a * b$

### Dowód

Zauważamy, że $b_i = b \cdot 2^i$.
$\sum_{i=0, a_i nieparzyste}^k b_i = \sum_{i=0, a_i nieparzyste}^{k} b \cdot 2^i=b \cdot\sum_{i=0, a_i nieparzyste}^{k} 2^i$
Kolejna obserwacja, którą należy poczynić, to że jeśli $a_i$ jest nieparzyste, to i'ty bit w binarnej reprezentacji $a$, licząc od najmłodszego, jest zapalony (bo proces znajdowania $a_i$ jest częścią wyznaczania zapisu liczby w postaci binarnej). Dlatego $\sum_{i=0, a_i nieparzyste}^{k} 2^i = a$.

Czyli $b \cdot\sum_{i=0, a_i nieparzyste}^{k} 2^i = b \cdot a$

### Pseudokod

```dupa=
Procedure russian_multiplication(a,b)
    pow <- 1
    result <- 0
    while a > 0 do
        if a mod 2 != 1 do
            result <- result + b * pow
        pow <- pow * 2
        a <- a / 2
```

### Złożoność


#### Czasowa

##### Kryterium jednorodne kosztów

Instrukcje wewnątrz pętli kosztują nas stałą liczbę operacji w maszynie ram, a sama pętla obróci się $O(log_2{a})$ razy, bo w każdym obrocie dzielimy $a$ przez $2$.

##### Kryterium logarytmiczne kosztów

Oprócz tego, że pętla obraca się $O(log_2{a})$ razy, to wewnątrz niej wykonujemy operacje na liczbach $a$ i $b$. Operacje na $a$ kosztują nas $O(log_2{a})$ operacji. Zauważamy, że w każdym obrocie pętli $result < b \cdot pow$ oraz $pow$ jest mniejsza lub równa początkowemu $a$ zatem ta operacja będzie nas kosztować $O(log_2{ab})$ i będzie to najdroższa operacja w pętli. Zatem całość będzie mieć złożoność asymptotyczną $O(log_2{a} \cdot log_2{ab})$

#### Pamięciowa

##### Kryterium jednorodne kosztów

Potrzebujemy stałej liczby zmiennych, czyli O(1).

##### Kryterium logarytmiczne kosztów

Wciąż potrzebujemy stałej liczby zmiennych, ale musimy oszacować najdłuższą w zapisie binarnym z nich. Będzie to wynik po wszyskich iteracjach. Skoro w każdej iteracji $result  < b \cdot pow$ oraz $pow$ jest mniejsze od początkowego $a$ to będzie miała ona długość $O(log_2{ab})$.
# ASK -- Lista 11
###### tags: `ASK`

## Zadanie 1

Używane będą 32-bitowe adresy, w których

* starsze 22 bity to tag
* młodsze 5 bitów to offset
* pozostałe 5 środkowych bitów to indeks wiersza

### Jaki jest rozmiar bloku?

Każdy bajt bloku adresujemy za pomocą offsetu, zatem skoro mamy do dyspozycji 5 bitów adresu, to musi ich być 32. 32-bitowe słowo ma 4 bajty długości, czyli na jeden blok przypadnie 8 takich słów.

### Ile wierszy ma nasza pamięć podręczna?

Do wierszy będziemy odnosić się za pomocą 5 bitów, zatem również każdej liczbie jaką możemy na nich zapisać będzie odpowiadał wiersz, czyli będzie ich 32.

### Jaki jest stosunek metadanych do danych?

Dla każdego bloku będziemy musieli przechować tag, na podstawie którego je rozróżnimy i bit valid, żeby móc rozpoznać czy jest to faktycznie pamięć której poszukujemy. Daje to 23 bity metadanych w stosunku do $256 + 23 = 279$ wszystkich bitów.

$$
\frac{23}{279} \cdot 100 \% \approx 8.24\%
$$

## Zadanie 2

Mamy zadany stan pamięci podręcznej systemu, w którym używa się 12-bitowego adresowania bajtowego. Pamięć podzielona jest na cztery zbiory, każdy z nich ma po dwa bloki, a każdy blok ma 4 bajty.

| Zbiór  | Znacznik | Valid | B0  | B1  | B2  | B3  |
|:------:|:--------:|:-----:|:---:|:---:|:---:|:---:|
|   0    |    00    |   1   | 40  | 41  | 42  | 43  |
|   0    |    83    |   1   | FE  | 97  | CC  | D0  |
|   1    |    00    |   1   | 44  | 45  | 46  | 47  |
|   1    |    83    |   0   |  -  |  -  |  -  |  -  |
|   2    |    00    |   1   | 48  | 49  | 4A  | 4B  |
|   2    |    40    |   0   |  -  |  -  |  -  |  -  |
|   3    |    FF    |   1   | 9A  | C0  | 03  | FF  |
|   3    |    00    |   0   |  -  |  -  |  -  |  -  |

### Format adresów

Mamy cztery zbiory, żeby każdy z nich mógł odpowiadać adresowi potrzebujemy `index` o długości dwóch bitów.

Każdy blok ma 4 bajty i musimy umieć je adresować, zatem potrzebny będzie `offset` długości dwóch bitów.

Resztę bitów, czyli 8 najstarszych, przeznaczymy na `tag`, czyli cały adres będzie miał postać $(tag, index, offset) = (addr_{11...4}, addr_{3...2}, addr_{1...0})$

### Odczyty

Mamy zdecydować czy dane próby odczytu zakończą się trafieniem czy chybieniem i jakie wartości zostaną wczytane.

| Adres | Tag | Zbiór | Offset | Trafienie? | Wartość |
|:-----:|:---:|:-----:|:------:|:----------:|:-------:|
|  832  | 83  |   0   |   2    |    Tak     |  CCD0   |
|  835  | 83  |   1   |   1    |    Nie     |    -    |
|  FFD  | FF  |   3   |   1    |    Tak     |  C003   |

## Zadanie 3

Rozważamy pamięć z poprzedniego zadania, zatem adresy będą dwunastobitowe w postaci $(tag, index, offset) = (addr_{11...4}, addr_{3...2}, addr_{1...0})$, a sama pamięć będzie podzielona na 4 zbiory po dwa wiersze. Zaczynamy od pustej pamięci, rozważamy ciąg odwołań zadanych liczbami w sytemie szesnastkowym.

```
0 4 10 84 3c e8 c8c a0 4 400 84 10 e8 884 c8c 0
```
### Polityka wymiany

Polityka wymiany to NRU. Zazwyczaj w tej polityce trzymamy dwa bity: \(R\)eferenced i (M)odified, które klasyfikują blok do jednej z 4 klas:

| Klasa | R | M |
|:-----:|:-:|:-:|
|   0   | 0 | 0 |
|   1   | 0 | 1 |
|   2   | 1 | 0 |
|   3   | 1 | 1 |

Jeśli występuje konflikt to na ofiarę zostanie wybrany losowo jeden z bloków posiadających najniższy numer klasy.

Bity są czyszczone w określonych odstępach czasu, tak żeby dotyczyły tylko ostatniego okresu czasu, w poniższym przykładzie przyjąłem że bity dla bloków zestawu zostaną wyczyszczone bezpośrednio przed dostępem do niego.

Podobnie w poniższym przykładzie nie mamy informacji o rozdzaju dostępu, dlatego pominiemy bit M.

### Odwołania

Zaczynamy od pustej pamięci.


| Adres | Tag | Zbiór | Offset | Trafienie? | Typ chybienia | Tag ofiary |
|:-----:|:---:|:-----:|:------:|:----------:|:-------------:|:------:|
|  000  | 00  |   0   |   0    |    Nie     |  Przymusowe   |   -    |
|  004  | 00  |   1   |   0    |    Nie     |  Przymusowe   |   -    |
|  010  | 01  |   0   |   0    |    Nie     |  Przymusowe   |   -    |
|  084  | 08  |   1   |   0    |    Nie     |  Przymusowe   |   -    |
|  03c  | 03  |   3   |   0    |    Nie     |  Przymusowe   |   -    |
|  0e8  | 0e  |   2   |   0    |    Nie     |  Przymusowe   |   -    |
|  c8c  | c8  |   3   |   0    |    Nie     |  Przymusowe   |   -    |
|  0a0  | 0a  |   0   |   0    |    Nie     |  Konflikt     |  00    |
|  004  | 00  |   1   |   0    |    Tak     |       -       |   -    |
|  400  | 40  |   0   |   0    |    Nie     |  Konflikt     |  01    |
|  084  | 08  |   1   |   0    |    Tak     |       -       |   -    |
|  010  | 01  |   0   |   0    |    Nie     |  Konflikt     |  0a    |
|  0e8  | 0e  |   2   |   0    |    Tak     |       -       |   -    |
|  884  | 88  |   1   |   0    |    Nie     |  Konflikt     |  00    |
|  c8c  | c8  |   3   |   0    |    Tak     |       -       |   -    |
|  000  | 00  |   0   |   0    |    Nie     |  Konflikt     |  40    |

#### Odpowiedzi na pytania z zadania

* Liczba wierszy zastąpionych z powodu chybienia wywołanego konfliktem: 5
* Liczba chybień przymusowych: 7
* Efaktywność:
	* Wliczając przymusowe chybienia: $\frac{4}{16} \cdot 100\% = 25\%$
	* Nie wliczając przymusowych chybień: $\frac{4}{9} \cdot 100\% \approx 44,4\%$

### Stan pamięci podręcznej po wykonaniu dostępów

#### Stan pamięci po pierwszej serii przymusowych chybień

| Zbiór  | Znacznik | Valid | Referenced |
|:------:|:--------:|:-----:|:----------:|
|   0    |   00     |   1   |     0      |
|   0    |   01     |   1   |     1      |
|   1    |   00     |   1   |     0      |
|   1    |   08     |   1   |     1      |
|   2    |   0e     |   1   |     1      |
|   2    |    -     |   0   |     0      |
|   3    |   03     |   1   |     0      |
|   3    |   c8     |   1   |     1      |

#### Stan końcowy

| Zbiór  | Znacznik | Valid | Referenced |
|:------:|:--------:|:-----:|:----------:|
|   0    |   01     |   1   |     0      |
|   0    |   00     |   1   |     1      |
|   1    |   88     |   1   |     1      |
|   1    |   08     |   1   |     0      |
|   2    |   0e     |   1   |     1      |
|   2    |    -     |   0   |     0      |
|   3    |   03     |   1   |     0      |
|   3    |   c8     |   1   |     1      |

#### Kandydaci na kolejne ofiary

* Zbiór 0: blok o tagu 01
* Zbiór 1: blok o tagu 08
* Zbiór 2: wolny blok
* Zbiór 3: blok o tagu 03

### Ile bitów na zbiór potrzeba do przechowania informacji o następnej ofiarze?

W ogólności: po dwa bity na każdy blok znajdujący się w wierszu. Przy specyfikacji przyjętej w zadaniu mamy dwa możliwe stany, albo do pierwszego wiersza się odwoływaliśmy się ostatnio, albo do drugiego, zatem wystarczyłby jeden bit na zbiór.

## Zadanie 4

Rozważamy pamięć w pełni asocjacyjną, czyli posiadającą pojedynczy zbiór zawierający wszystkie bloki, których w tym przypadku będzie osiem. Adresy będą dwunastobitowe w postaci $(tag, offset) = (addr_{11...2}, addr_{1...0})$. 

### Polityka wymiany

W tym przypadku polityką wymiany będzie LRU (ang. *Least Recently Used*), czyli na ofiarę wybrany będzie blok, do którego od najdłuższego czasu nie został wykonany dostęp.

### Odwołania

Przeanalizujemy ciąg odwołań zadany liczbami w systemi szesnastkowym:
```
0 4 10 84 3c e8 c8c a0 4 400 84 10 e8 884 c8c 0
```

Zaczynamy od pustej pamięci.


| Adres | Znacznik -- pierwszy bajt | Znacznik -- pozostałe dwa bity | Offset | Trafienie? | Typ chybienia | Lp. ofiary |
|:-----:|:---:|:-----:|:------:|:----------:|:-------------:|:------:|
|  000  | 0x00  | 0b00 | 0 |  Nie  |  Przymusowe   |   -    |
|  004  | 0x00  | 0b01 | 0 |  Nie  |  Przymusowe   |   -    |
|  010  | 0x01  | 0b00 | 0 |  Nie  |  Przymusowe   |   -    |
|  084  | 0x08  | 0b01 | 0 |  Nie  |  Przymusowe   |   -    |
|  03c  | 0x03  | 0b11 | 0 |  Nie  |  Przymusowe   |   -    |
|  0e8  | 0x0e  | 0b10 | 0 |  Nie  |  Przymusowe   |   -    |
|  c8c  | 0xc8  | 0b11 | 0 |  Nie  |  Przymusowe   |   -    |
|  0a0  | 0x0a  | 0b00 | 0 |  Nie  |  Przymusowe   |   -    |
|  004  | 0x00  | 0b01 | 0 |  Tak  |       -       |   -    |
|  400  | 0x40  | 0b00 | 0 |  Nie  |  Brak miejsca |   1    |
|  084  | 0x08  | 0b01 | 0 |  Tak  |       -       |   -    |
|  010  | 0x01  | 0b00 | 0 |  Tak  |       -       |   -    |
|  0e8  | 0x0e  | 0b10 | 0 |  Tak  |       -       |   -    |
|  884  | 0x88  | 0b01 | 0 |  Nie  |  Brak miejsca |   5    |
|  c8c  | 0xc8  | 0b11 | 0 |  Tak  |       -       |   -    |
|  000  | 0x00  | 0b00 | 0 |  Nie  |  Brak miejsca |   8    |

#### Odpowiedzi na pytania z zadania

* Liczba wierszy zastąpionych z powodu chybienia wywołanego konfliktem: 0
* Liczba wierszy zastąpionych z powodu chybienia wywołanego brakiem miejsca w pamięci (ang. *capacity misses*: 3
* Liczba chybień przymusowych: 8
* Efaktywność:
	* Wliczając przymusowe chybienia: $\frac{5}{16} \cdot 100\% = 21.25\%$
	* Nie wliczając przymusowych chybień: $\frac{5}{8} \cdot 100\% = 62.5\%$

### Stan pamięci podręcznej po wykonaniu dostępów

#### Stan pamięci po pierwszej serii przymusowych chybień

| Lp. | Znacznik -- pierwszy bajt | Znacznik -- pozostałe bity | Valid |    Wiek    |
|:-:|:--------:|:----:|:-----:|:----------:|
| 1 |   0x00   | 0b00 |   1   |     8      |
| 2 |   0x00   | 0b01 |   1   |     7      |
| 3 |   0x01   | 0b00 |   1   |     6      |
| 4 |   0x08   | 0b01 |   1   |     5      |
| 5 |   0x03   | 0b11 |   1   |     4      |
| 6 |   0x0e   | 0b10 |   1   |     3      |
| 7 |   0xc8   | 0b11 |   1   |     2      |
| 8 |   0x0a   | 0b00 |   1   |     1      |

#### Stan końcowy

| Lp. | Znacznik -- pierwszy bajt | Znacznik -- pozostałe bity | Valid |    Wiek    |
|:-:|:--------:|:----:|:-----:|:----------:|
| 1 |   0x04   | 0b00 |   1   |     7      |
| 2 |   0x00   | 0b01 |   1   |     8      |
| 3 |   0x01   | 0b00 |   1   |     5      |
| 4 |   0x08   | 0b01 |   1   |     6      |
| 5 |   0x88   | 0b01 |   1   |     3      |
| 6 |   0x0e   | 0b10 |   1   |     4      |
| 7 |   0xc8   | 0b11 |   1   |     2      |
| 8 |   0x00   | 0b00 |   1   |     1      |

#### Kandydaci na kolejne ofiary

Mamy jeden zbiór i kandydatem jest wiersz oznacznony numerem 2.

### Ile bitów na zbiór potrzeba do przechowania informacji o następnej ofiarze?

Rozważmy zbiór, w którym znajdują się 3 bloki. Jeden z nich musi być tym użytym ostatnio, spośród dwóch pozostałych jeden musi być być tym użytym przedostatnio, co zostawi jeden blok, który był użyty najdawniej. Stanów w tym przypadku jest 3!, ale rozumowanie to można uogólnić na dowolną liczbę wierszy na zbiór. W ogólności: dla $n$ bloków  w zbiorze jest to $n!$ stanów. Zatem na zareprezentowanie każdego z nich potrzeba $\left\lceil log_2(n!) \right\rceil$ bitów.

## Zadanie 5

### Dlaczego nie należy używać najstarszych bitów do wyboru zbioru?

Gdybyśmy używali starszych bitów do wybierania zbioru, mogłaby się zdarzyć sytuacja, w której obszary pamięci leżące względnie blisko siebie mapowane byłyby do tego samego zbioru, co zaprzepaściłoby korzyści jakie mamy w przeciwnym razie przy programach, które przeglądają dane sekwencyjnie.

Kiedy używamy młodszych bitów (zaraz przed offsetem) obszary pamięci leżące blisko siebie mapowane są do różnych zbiorów, dzięki czemu wszystkie mogą jednocześnie przebywać w pamięci podręcznej.

Sytuacja ta jest dobrze zobrazowana w podręczniku:

![](https://i.imgur.com/38XEK8n.png)



### Skąd podział pamięci podręcznej L1?

Pamieć podręczna na poziomie L1 jest podzielona na dwa moduły, jeden przechowujący wyłącznie dane, a drugi -- instrukcje. Niektóre z korzyści jakie przynosi taka organizacja pamięci to:

* Możliwość równoległego odczytu z pamięci danych i instrukcji w jednym cyklu, co jest szczególnie przydatne w procesorach, które wykorzystują potokowość (ang. *pipelining*) w wykonywaniu instrukcji.
* Pamięć instrukcji może zostać zaimplementowana jako pamięć tylko do odczytu, co potencjalnie uprości układ.
* Możliwość dostosowania parametrów pamięci, czy polityki wymiany, pod wzorce dostępów charakterystyczne dla danych lub instrukcji (zazwyczaj są one różne).
* Dodatkowo podziałem pamięci zapewniamy, że dane i instrukcje, które zazwyczaj leżą pod odległymi od siebie adresami, nie będą konkurowały o miejsce w pamięci powodując chybienia wywołane konfilktem.

## Zadanie 6

### Polityka zapisu

Stosujemy polityki **write-back** i **write-allocate**.

* write-back -- Uaktualniamy wartość w wierszu pamięci podręcznej niższego poziomu, dopiero kiedy wiersz jest usuwany z pamięci z wyższego poziomu. (zmodyfikowanie wiersza sygnalizujemy bitem *dirty*).
* write-alocate -- Jeśli wiersza pamięci nie ma na danym poziomie pamięci podręcznej, to najpierw załadujemy go do niej i dopiero zaktualizujemy jego wartość

### Schemat blokowy

![](https://i.imgur.com/Q0IasDj.png)


## Zadanie 7

Dane:

* 36% instrukcji to instrukcje dostępu do pamięci
* Dostęp do pamięci głównej trwa $70ns$.
* Dostęp do pamięci podręcznej L1 trwa $0.66ns$ (jeden cykl), a współczynnik chybień wynosi w niej $8.0\%$.
* Czyli 1 cykl to $0.66ns$.
* Dostęp do pamięci podręcznej L2 trwa $5.62ns$, a współczynnik chybień wynosi w niej $0.5\%$.
* Inne instrukcje wykonują się w jednym cyklu.

### Jaki jest średni czas dostępu do pamięci?

W obu przypadkach wprowadzimy dyskretną zmienną losową $X$, która oznaczać będzie możliwe długości czasu dostępu do pamięci.

#### Używając tylko L1

Opiszemy rozkład $X$ tabelką:

| $i$ | $x_i$ | $P(X = x_i)$ |
|:--:|:-----:|:-----:|
| $1$ |$0.66$ |$0.92$ |
| $2$ |$70.66$|$0.08%$|

Chcemy policzyć wartośc oczekiwaną
$$
E(X) = \sum_{x_i}x_iP(X=x_i) = 0.66 \cdot 0.92 + 70.66 \cdot 0.08 = 6.26ns
$$

Wiedząc ile nanosekund trwa cykl, możemy również powiedzieć że średnia liczba cykli na dostęp to $\frac{6.26}{0.66}  = 9.48$ cykla.

#### Używając L1 i L2

Do tabelki dojdzie nowa możliwość:

| $i$ | $x_i$ | $P(X=x_i)$ |
| :--:|:-----:|:-----:|
| $1$ |$0.66$ |$0.92$ |
| $2$ |$6.28$ |$0.08 \cdot 0.995 = 0.0796$  |
| $3$ |$76.28$|$0.08 \cdot 0.005 = 0.0004 %$|

Tak samo jak poprzednio liczymy wartość oczekiwaną
$$
E(X) = \sum_{x_i}x_iP(X=x_i) = 0.66 \cdot 0.92 + 6.28 \cdot 0.0796 +
76.28 \cdot 0.0004 = 1.1376ns
$$

Oraz liczbę cykli: $\frac{1.1376}{0.66} \approx 1.72$ cykla.

### Jakie jest CPI?

Podobnie jak wyżej będziemy chcieli policzyć średnią liczbę cykli na instrukcję, czyli inaczej wartość oczekiwaną ze zmiennej $Y$ oznaczającej liczbę cykli na instrukcję.

#### Używając tylko L1

Opiszmy rozkład $Y$ tabelką:

| $i$ | $y_i$ | $P(Y = y_i)$ |
|:---:|:-----:|:-----:|
| $1$ |$1$   |$0.64$ |
| $2$ |$9.48$|$0.36%$|

Następnie policzymy wartość oczekiwaną

$$
E(Y) = \sum_{y_i}y_iP(Y = y_i) = 1 \cdot 0.64 + 9.48 \cdot 0.36 \approx 4.05 \text{ cykla} = \text{CPI}
$$

#### Używając L1 i L2

Tabelka ulegnie zmianie:

| $i$ | $y_i$ | $P(Y = y_i)$ |
|:---:|:-----:|:-----:|
| $1$ |$1$   |$0.64$ |
| $2$ |$1.72$ |$0.36%$|

Wartość oczekiwaną liczmy tak samo

$$
E(Y) = \sum_{y_i}y_iP(Y = y_i) = 1 \cdot 0.64 + 1.72 \cdot 0.36 \approx 1.26 \text{ cykla} = \text{CPI}
$$

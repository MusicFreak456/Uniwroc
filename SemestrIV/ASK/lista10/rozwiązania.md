# ASK -- Lista 10
###### tags: `ASK`


## Zadanie 1

Dane:

* Jeden plater
* Jedna głowica
* 400 tysięcy ścieek na powierzchnię
* 2500 sektorów na ścieżkę
* 7200 obrotów na minutę (RPM)
* 1ms na przeskoczenie 50 tysięcy ścieżek

### Jaki jest średni czas wyszukiwania?

Jest on zależny od odległości pomiędzy ścieżką, na której znajduje się głowica, a ścieżką docelową. Możemy oszacować tą wartość losując pozycję startową głowicy i pozycję docelową kilka tysięcy razy oraz biorąc średnią z otrzymanych czasów. 

![](https://i.imgur.com/2WDcHZr.png)

Wartości oscylują w okolicy 2,669ms, zatem
$$
T_{avg\ seek} \approx 2,669ms
$$

### Jaki jest średni czas opóźnienia obrotowego?

Korzystając ze wzorów z podręcznika:

$$
T_{avg\ rotation} = \frac{1}{2}T_{max\ rotation},
$$

gdzie

$$
T_{max\ rotation} = \frac{1}{RPM} \times \frac{60s}{1min}.
$$

Więc podstawiamy do drugiego wzoru:
$$
T_{max\ rotation} = \frac{1}{7200} \times \frac{60}{1} = \frac{60}{7200}.
$$
I do pierwszego:
$$
T_{avg\ rotation} = \frac{1}{2} \times \frac{60}{7200} = \frac{60}{14400} \approx 0,004s = 4ms ,
$$

### Jaki jest czas transferu sektora?

Ponownie, korzystamy z wzorów z podręcznika:

$$
T_{avg\ transfer} = \frac{1}{RPM} \times \frac{1}{(average\ \#\ sectors/track) } \times \frac{60s}{1min}
$$

W naszym przypadku liczba sektorów na ścieżkę jest stała, równa 2500. Podstawiamy

$$
T_{avg\ transfer} = \frac{1}{7200} \times \frac{1}{2500} \times \frac{60}{1} = \frac{60}{18 000 000} \approx  0,000003s = 0,003 ms
$$

### Jaki jest całkowity średni czas obsługi zdarzenia?

Zgodnie z podręcznikiem całkowity czas obsługi to suma wyliczonych wyżej wartości

$$
T_{access} = T_{avg\ seek} + T_{avg\ rotation} + T_{avg\ transfer} = \\
2,669 + 4 + 0,003 \approx 6,67ms
$$

## Zadanie 2

Dane:

Specyfikacja dysku:
* 360 RPM
* 512 bajtów na sektor
* 96 sektorów na ścieżkę
* 110 ścieżek na powierzchnię

Dysk sygnalizuje dostępność danych zgłaszając przerwanie na każdy przeczytany bajt. Wykonanie procedury przerwania zajmuje procesorowi $2,5 \mu s$.

### Bez kontrolera DMA

Mamy zignorować czas wyszukiwania ścieżki oraz sektora, zatem jedyne co nas interesuje to czas transferu. Dla jednego sektora:

$$
T_{avg\ transfer} = \frac{1}{RPM} \times \frac{1}{(average\ \#\ sectors/track) } \times \frac{60s}{1min} 
= \frac{1}{360} \times \frac{1}{96} \times \frac{60}{1} = \\ = \frac{60}{34560}s \approx 0,00173611s = 1736,11 \mu s
$$

Zatem dla $n$ sektorów transfer zajmuje $1736,11\cdot n\ \mu s$. Ten czas mógłby być przeznaczony na wykonywanie przez procesor innych czynności niezwiązanych z dostępem do pamięci, jednak część z niego będzie musiał poświęcić na obsługę przerwań. Dla jednego sektora (który nie był pierwszy)

$$
	t_s = 512 \cdot 2,5 = 1280 \mu s
$$

Zatem dla $n$ sektorów będzie to

$$
	t = ((1736,11 - 1280) \cdot n + 2,5 ) \mu s
$$

Dodajemy 2,5 ze względu na to, że w pierwszym bajcie przerwanie nie podkradnie nam wolnego czasu.

### Korzystając z kontrolera DMA

Sytuacja wyglądać będzie podobnie jak w powyższych rozważaniach, jedynie przerwanie procesor otrzymywać będziemy teraz tylko przy wczytaniu sektora, co drastycznie zmiejszy czas spędzony w procedurze przerwania

$$
	t = ((1736,11 - 2,5) \cdot n + 2,5 ) \mu s
$$

## Zadanie 4

Dane:

* Taktowanie procesora 2,5GHz
* Trzy poziomy pamięci podręcznej L1, L2, L3

Wartości funkcji $A(k)$ wyznaczającej czas w cyklach potrzebny na odpowiedź pamięci $k$ na zapytanie czy szukany blok pamięci się w niej znajduje.

|  x   | A(x) |
|:----:|:----:|
|  L1  |  4   |
|  L2  |  12  |
|  L3  |  40  |
| DRAM | 200  |

Wartości funkcji $H(k)$ zwracającej prawdopodobieństwo z jakim szukany blok pamięci znajduje się na poziomie $k$

|  x   | H(x) |
|:----:|:----:|
|  L1  | 0.9  |
|  L2  | 0.95 |
|  L3  | 0.98 |
| DRAM |  1   |

### Średni czas dostępu

Niech zmienna losowa $X$ reprezentuje liczbę cykli procesora potrzebną na zrealizowanie dostępu do pamięci. Policzymy jej wartość oczekiwaną. Niech $V = \{4, 16, 56, 256\}$ będzie możliwymi wartościami liczby cykli.

$$
	E(X) = \sum_{x \in V} x \cdot P(X = x) = \\ = 4 \cdot 0.9 + 16 \cdot 0.1 \cdot 0.95 + 56 \cdot 0.1 \cdot 0.05 \cdot 0.98 + 256 \cdot 0.1 \cdot 0.05 \cdot 0.02 = \\
	= 5.42
$$

Zatem średnio spodziewamy się około 5.42 cykla na wykonanie dostępu do pamięci. Teraz należy wyznaczyć jaki to czas

$$
 t = \frac{5.42}{2.5 \cdot 10^9} = 2.168 \cdot 10^{-9} s = 2,168 ns
$$

### Pesymistyczny przypadek

W pesymistycznym przypadku nie trafiamy w żaden z poziomów pamięciu podręcznej i dane znajdujemy dopiero na poziomie DRAMu, więc po spaleniu 256 cykli.

$$
t = \frac{256}{2.5 \cdot 10^9} = 102.4 \cdot 10^{-9} s = 102.4 ns
$$

## Zadanie 7

Mamy policzyć ile nanosekund zajmie sprowadzenie 64 bajtowego bloku pamięci podręcznej z pamięci DRAM dla dwóch wyspecyfikowanych modułów.

### Moduł pierwszy

* DDR4-1600
* $t_{CLK}$ = 800MHz
* $t_{CAS}$ = 10
* $t_{RCD}$ = 10
* $t_{RP}$  = 10
* $t_{RAS}$ = 25

Mamy do sprowadzenia 64 bajty, czyli 8 słów. Pracując na module `DDR4` mamy prefetch rozmiaru 8 słów, który pozwala nam pobrać 8 kolumn z wiersza bez ponownego wybierania adresu wiersza. Szerokość kolumny to jedno słowo. Zakładamy że jesteśmy w stanie w jednym cyklu przenieść 1 słowo do procesora.

Obliczenia oparte są na poniższym schemacie

![](https://i.imgur.com/m38YzNz.png)

gdzie przez CL jest oznaczony nasz czas $t_{CAS}$.

#### Bez trybu sekwencyjnego

$$
t_c = t_{RP} + t_{RCD} + 8 \cdot t_{CAS} + 8 = \\
= 10 + 10 + 80 + 8 = 108
$$

Znając liczbę cykli i taktowanie możemy policzyć czas

$$
t = \frac{108}{800 \cdot 10^6} = 0,135 \cdot 10^{-6}s = 135ns 
$$

#### W trybie sekwencyjnym

W tym trybie pamięć nie będzie czekać na polecenie zmiany kolumny, tylko od razu poda 16 kolejnych słów.

$$
t_c = t_{RP} + t_{RCD} + t_{CAS} + 16 = \\
= 10 + 10 + 10 + 16 = 46
$$

$$
t = \frac{46}{800 \cdot 10^6} = 0,0575 \cdot 10^{-6}s = 57.5ns 
$$

### Moduł drugi

Powtarzamy obliczenia dla innych danych:

* DDR4-2133
* $t_{CLK}$ = 1066,67MHz
* $t_{CAS}$ = 15
* $t_{RCD}$ = 15
* $t_{RP}$  = 15
* $t_{RAS}$ = 36

#### Bez trybu sekwencyjnego

$$
t_c = t_{RP} + t_{RCD} + 8 \cdot t_{CAS} + 8 = \\
= 15 + 15 + 120 + 8 = 158
$$

$$
t = \frac{158}{1066,67 \cdot 10^6} \approx 0,148 \cdot 10^{-6}s = 148ns 
$$

#### W trybie sekwencyjnym

W tym trybie pamięć nie będzie czekać na polecenie zmiany kolumny, tylko od razu poda 16 kolejnych słów.

$$
t_c = t_{RP} + t_{RCD} + t_{CAS} + 16 = \\
= 15 + 15 + 15 + 16 = 61
$$

$$
t = \frac{61}{1066,67 \cdot 10^6} \approx 0,0571 \cdot 10^{-6}s = 57.1ns 
$$


## Zadanie 8

Mamy policzyć ile czasu zajmie sprowadzenie tablicy o rozmiarze 4GiB umieszczonej pod adresem podzielnym przez $2^{20}$.

Wykorzystujemy moduł pamięci z poprzedniego zadania, o specyfikacji:

* DDR4-2133
* $t_{CLK}$ = 1066,67MHz
* $t_{CAS}$ = 15
* $t_{RCD}$ = 15
* $t_{RP}$  = 15
* $t_{RAS}$ = 36

4GiB to $4 \cdot 2^{30}$ bajtów, czyli $2^{32}$ bajtów. Pojedynczy wiersz ma 8KiB, czyli $8 \cdot 2^{10} = 2^{13}$ bajtów. Zatem będziemy musieli pobrać $2^{19}$ wierszy.

Możemy korzystać z transferu sekwencyjnego i pobierać po 16 słów z wiersza bez wykonywania zmian kolumny. 16 słów to $16 \cdot 2^{3} = 2^{7}$ bajtów, czyli żeby przeczytać cały wiersz musimy wykonać $2^{6}$ odczytów sekwencyjnych.

### Liczba cykli jednego odczytu sekwencyjnego

Czas jednego oczytu to czas $t_{CAS}$ plus czas potrzebny na wczytanie 16 słów. Załóżmy jak w poprzednim zadaniu, że przesłanie jednego słowa 64-bitowego zajmuje jeden cykl.

$$
t_s = t_{CAS} + 16 = 31
$$

### Liczba cyki jednego odczytu wiersza

Składać na niego będzie się czas przygotowania wiersza $t_{RP}$ (precharge), czas pomiędzy odczytem adresu kolumny, a odczytem adresu wiersza $t_{RCD}$ oraz czas na wykonie  $2^{6}$ odczytów sekwencyjnych.

$$
t_w = t_{RP} + t_{RCD} + 2^{6} \cdot t_s = 15 + 15 + 2^6\cdot 31 = 2014
$$

### Czas przesłania danych

Znając taktowanie i liczbę cykli na odczyt jednego wiersza możemy policzyć ile sekund zajmie transfer

$$
t = \frac{2014 \cdot 2^{19}}{1066.67 \cdot 10^{6}} \approx 0.9899s
$$

### Przypadek pamięci dwukanałowej

Konfiguracja ta podwaja przepustowość między pamięcią, a kontrolerem pamięci poprzez używanie dwóch zestawionych ze sobą modułów pamięci.

Zatem mamy teraz możliwość przesłania dwa razy więcej bajtów w jednym odczycie, czyli potrzebne będzie dwa razy mniej odczytów sekwencyjnych. Czas na przesłanie danych do procesora to wciąż będzie 1 cykl na 64 bity, więc czas jednego odczytu sekwencyjnego wzrośnie, ale ich liczba zmaleje.

$$
t_s = t_{CAS} + 32 = 47
$$

Liczba cykli potrzebnych na wczytanie wiersza z kolei zmaleje

$$
t_w = t_{RP} + t_{RCD} + 2^{5} \cdot t_s = 15 + 15 + 2^5\cdot 47 = 1534
$$

Zatem łączny czas też

$$
t = \frac{1534 \cdot 2^{19}}{1066.67 \cdot 10^{6}} \approx 0.7539s
$$
# Raport do zadania 1
###### tags: `ASK`

### Autor: Cezary Świtała
### Numer indeksu: 316746

Konfiguracja
---

Informacje o systemie:

 * Dystrybucja: Linux Mint 20.1 x86_64
 * Jądro systemu: 5.4.0-72-generic
 * Kompilator: GCC 9.3.0
 * Procesor: Intel(R) Core(TM) i5-4460  CPU @ 3.20GHz
 * Liczba rdzeni: 4

Pamięć podręczna:

 * L1d: 32 KiB, 8-drożny (per rdzeń), rozmiar linii 64B
 * L2: 256 KiB, 8-drożny (per rdzeń), rozmiar linii 64B
 * L3: 6 MiB, 12-drożny (współdzielony), rozmiar linii 64B

Pamięć TLB:

 * L1d: 4KiB strony, 4-drożny, 64 wpisy
 * L2: 4KiB strony, 8-drożny, 1024 wpisów

Informacje o pamięciach podręcznych uzyskano na podstawie wydruku programu
`lscpu` oraz strony cpu-world.com (program `x86info` nie rozpoznawał tego procesora) .

Wyniki eksperymentów
---

### Powtórzenie eksperymentu z wykładu

Zbadamy wydajność trzech różnych wersji mnożenie dwóch macierzy rozmiaru `n x n`. Eksperyment będzie polegał na uruchomieniu różnych wersji mnożenia macierzy dla rosnącego `n`. Jako że brakuje nam narzędzi do badania cykli w pojedynczym obrocie pętli, załóżmy że mają one odzwierciedlenie w czasie działania programu. Aby zbadać średni czas działania, odpowiednie wersje mnożenia, dla ustalonego `n`, uruchomimy kilkadziesiąt razy, a następnie odrzucimy 10% skrajnych wartości, po czym weźmiemy średnią z pozostałych. 

Do wygenerowania poniższego wykresu użyte zostały dane z pliku `versions.dat` wygenerowane przez procedurę `benchmark_versions` z pliku `benchmark.py` (który zawiera cały kod automatyzujący napisany przeze mnie na tą pracownię). Skrypt programu gnuplot dla tego i innych wykresów dostępny jest w pliku `figure.gp`.

![](https://i.imgur.com/PuTqx8m.png)


#### Czy uzyskane wyniki różnią się od tych na slajdów?

W bardzo niewielkim stopniu. Zachowana została kolejność w procedur jeśli chodzi o średnią prędkość oraz widać również na nim charakterystyczne nagłe spadki wydajności w stosunku do lepszych procedur.

#### Z czego wynikają różnice w wydajności?

Różne wersje są w różnym stopniu zoptymalizowane pod względem dostępów do pamięci podręcznej. Wiedząc w jaki sposób ułożona jest w pamięci tablica, oraz że chybienie powoduje załadowanie do pamięci całego bloku, możemy przeanalizować co się dzieje przy wywołaniu tych procedur.

* Wersja `ijk(jik)` -- pożyczając grafikę z wykładu: 
![](https://i.imgur.com/coiHkgw.png)
Jak widać, ta nie najgorzej radząca sobie metoda, będzie wykazywać lokalność przestrzenną odwołując się sekwencyjnie do kolejnych elementów wiersza `A`, przez co zmniejszy liczbę chybień, gdyż wywoła jedno co długość bloku. Gorsza sytuacja jest w macierzy `B` gdzie będzie miało miejsce chybienie przy każdym kolejnym wierszu dla odpowiednio dużego `n`.


* Wersja `jki(kji)`:
![](https://i.imgur.com/bocpRp9.png)
Najgorsza możliwa wersja. Dla odpowiednio dużego `n` będziemy generować chybienia zarówno przechodząc po każdym elemencie kolumny macierzy `A` jak i `C`.

* Wersja `kij(ikj)`:
![](https://i.imgur.com/sl9eA3r.png)
Najlepsza wersja (nie licząc kafelków). Odwołujemy się sekwencyjnie zarówno do elementów macierzy B jak i C. Generując chybienia tylko przechądząc pomiędzy blokami pamięci podręcznej.

### Czy rozmiar kafelka ma znaczenie?

Przy mnożeniu macierzy metodą kafelkową, obliczamy pojedynczy kafelek macierzy wynikowej dosumowując wyniki mnożenia kafelków z macierzy `A` i `B`. Ważne jest zatem żeby zarówno kafelki z macierzy `A` i `B` jak i kafelek z`C` mogły jednocześnie znaleźć się w pamięci podręcznej, czyli kafelek musi być wystarczająco duży żeby zrobić różnicę w obliczeniach, ale nie zbyt duży.

#### Jaki wpływ ma wielkość kafelka na działanie?

Przetestujemy to eksperymentalnie dla kafelków wielkości od 2 do 32. Rozmiar boku macierzy ustalimy na 1024 (pod uwagę brane będą tylko potęgi dwójki, gdyż sprawdzanie może się odbyć tylko na wielkościach macierzy podzielnych przez długość kafelka, więc gdyby dodać więcej wartości to wspólna wielokrotność była by już bardzo duża i nie przeliczyłaby się w rozsądnym czasie). Ponieważ wymaga to edytowania kodu, wartości zmieniałem ręcznie. Testowanie średniej czasu odbywało się tak samo jak przy powtarzaniu eksperymentu z wykładu, za pomocą procedury `test_version` z pliku `benchmark.py`.

Dane z wykresu umieszczone zostały w pliku `blocks_size.dat`

![](https://i.imgur.com/DUuIiJG.png)

Eksperyment potwierdził wcześniejsze przypuszczenia. Widać, że wielkość kafelka ma wpływ na czas wykonania procedury `matmult3` oraz że wybieranie większych kafelków ma sens do pewnego momentu. Dodatkowo `16` daje nam najlepszy czas spośród rozważanych wartości.

### Czy wyzerowana wartość offsetu wpływa na wydajność

Mamy sprawdzić czy gdy wyzerujemy wartości `A_OFFSET`, `B_OFFSET`, `C_OFFSET` to wpłynie to negatywnie na wydajność metody z kafelkowaniem.

W tym celu zedytujemy wartości w pliku `mathmult`, następnie przeprowadzimy eksperyment analogicznie do tego co zrobiliśmy w pierwszym podpunkcie. Dane wykresu zostały umieszczone w pliku `blocks_offset.dat`.

![](https://i.imgur.com/Md663Aa.png)

Wyniki eksperymentu w ogólności nie wskazują na spadek wydajności przy ustawieniu wartości `*_OFFSET` na zero, poza małą anomalią przy rozmiarze `512 x 512`.

### Pytania bez opowiedzi

Niestety nie zdążyłem odpowiedzieć na pytania:

* Czy inny wybór wartości offset daje poprawę wydajności?
* Dla jakich wartości n obserwujesz znaczący spadek wydajności
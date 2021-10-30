## Zadanie 3

### Pseudokod

```pseudo=
Procedure bubble(T[1...n])
    for i <- 1 to n-1 do
        for j <- 1 to n-1-i do
            if T[j] > T[j+1] then swap(T[j], T[j+1])
```

### Idea
W i-tej iteracji wypychamy największy element spośród n-i+1 pierwszych elementów tablicy na n-i+1'sze miejsce. W ten sposób po n iteracjach mamy uporządkowaną rosnąco tablicę.

### Złożoność czasowa

Przy kryterium jednorodnym, porównanie elementów i ewentualna ich zamiana wymaga stałej liczby operacji. Wewnętrzna pętla w i-tej iteracji pętli zewnętrznej obraca się n-i razy.

**x** - W I'tej iteracji j przyjęło wartość J

|I \ J| 1 | 2 | 3 | ... | n-3 | n-2 | n-1 |
|-- | --- | -- | -- | --  | -- | -- | -- |
| 1 | x   | x  | x  | ... | x  | x  | x  |
| 2 | x   | x  | x  | ... | x  | x  |    | 
| 3 | x   | x  | x  | ... | x  |    |    | 
| ... |    |  |  |  |  |    |    | 
|n - 2 | x | x | | ... | | | | |
|n - 1 | x |  | | ... | | | | |

Widać że wewnętrzna pętla obróci się zatem n - 1 + (n-2) + ... + 2 + 1 razy, czyli (n-1) * n / 2 razy. Jest to także liczba porównań jaką wykonuje algorytm. Złożoność asymptotyczna tego algorytmu wynosi zatem O(n^2).

### Rozkład danych

Złożoność czasowa algorytmu nie jest uzależniona od uporządkowania tablicy wejściowej tak samo jak w algorytmie Select, jedyne co się zmienia to liczba zamian miejscami elementów, zatem w przypadku kiedy dane wejściowe częściej są prawie uporządkowane, lepszy okaże się insert.

### Wielkość rekordów

W sytuacji kiedy operacja zamiany miejscami elementów jest kosztowna, bubblesort również nie bedzie dobrym wyborem, gdyż wykonuje O(n^2) przestawień elementów, kiedy select wykonuje ich zawsze O(n).


### Stabilność

Bubble sort jest algorytmem stabilnym tak samo jak insert, a w przeciwieństwie do selecta.
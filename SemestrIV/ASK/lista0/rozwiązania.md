# ASK -- Lista 0

###### tags: `ASK`

## Zadanie 1

$42_{(8)} = 100010_{(2)} = 22_{(16)} = 34_{(10)}$
$255_{(8)} = 10101101_{(2)} = AD_{(16)} = 173_{(10)}$
$3047_{(8)} = 11000100111_{(2)} = 627_{(16)} = 1575_{(10)}$
$140336_{(8)} = 1100000011011110_{(2)} = C0DE_{(16)} = 49374_{(10)}$

## Zadanie 2

a)
|              |     |
| ------------ | ---:|
| przenisienie |     |
|              |  22 |
| +            |   8 |
| =            |  2A |

b)
|              |     |
| ------------ | ---:|
| przenisienie |     |
|              |  73 |
| +            |  2C |
| =            |  9F |
c)
|              |     |
| ------------ | ---:|
| przenisienie |  10 |
|              |  7F |
| +            |  7F |
| =            |  FE |
c)
|              |     |
| ------------ | ---:|
| przenisienie | 100 |
|              |  C2 |
| +            |  A4 |
| =            | 166 |
## Zadanie 3

```c=
uint32_t set_kth_to_zero(uint32_t x, uint32_t k) {
    return x & ~(1 << k);
}

uint32_t set_kth_to_one(uint32_t x, uint32_t k) {
    return x | (1 << k);
}

uint32_t negate_kth_bit(uint32_t x, uint32_t k){
    return x ^ (1 << k);
}
```

## Zadanie 4

```c=
uint32_t multiply_by_pow_of_two(uint32_t x, uint32_t y) {
    return x << y;
}

uint32_t divide_by_pow_of_two_floor(uint32_t x, uint32_t y) {
    return x >> y;
}

uint32_t mod_pow_of_two(uint32_t x, uint32_t y) {
    return x & ((1 << y) - 1);
}

uint32_t divide_by_pow_of_two_ceil(uint32_t x, uint32_t y) {
    return (x + ((1 << y) - 1)) >> y;
}
```

<!-- ## Zadanie 5

```c=
//Chyba jednak nielegalne
bool is_pow_of_two(uint32_t x) {
    return !(x == 0) && !(x & (x - 1));
}
``` -->

## Zadanie 6

**Big endian** - najbardziej znaczący bajt jest umieszczony na początku
**Little endian** - na odwrót

```c=
uint32_t little_to_big_endian(uint32_t x) {
    return
        (x >> 24) |
        ((x & 0x00FF0000) >> 8) |
        ((x & 0x0000FF00) << 8) |
        (x << 24);
}
```

## Zadanie 7

**ASCII** - popularny siedmiobitowy system kodowania znaków, w którym liczbom od 0 do 127 przypisuje się znaki i kody sterujące.

**Kod sterujący** - w przeciwieństwie do znaków drukowalnych, nie zostają one wyświetlone/wydrukowane, tylko sterują urządzeniem odbierającym dane, przykładem może być kod przeniesienia do nowej lini.

0 - NULL - oryginalnie reprezentował brak informacji, urządzenia miały na niego nie reagować.
Współcześnie używany na przykład w null-terminated-string'ach, w których oznacza ich koniec.

4 - EOT - oznacza koniec wprowadzania danych. Stosowany np. w programach które oczekują na wprowadzenie danych przez użytkownika. Koniec wprowadzania danych jest sygnalizowany programowi właśnie za pomocą EOT (ctrl + D).

7 - BELL - powoduje wydanie dźwięku, np. aby ostrzec operatora urządzenia o otrzymaniu wiadomości.

10 - EOL - sygnalizuje koniec bieżacego wiersza tekstu.

12 - FF - sygnalizuje koniec bieżącej strony tekstu. Np. w drukarkach powoduje przeniesienie się na kolejną kartkę.

## Zadanie 8

Głównym ograniczeniem ASCII była możliwość przedstawienia jedynie 128 znaków, co nie pozwalało na pokrycie znaków diaktrytycznych, powszechnie występujących w wielu systemach pisma. Z myślą o nich powstał UTF-8, w którym da się reprezentować wszystkie znaki ze standardu Unicode.

### Sposób kodowania

x'y to kolejne bity liczby w kodzie Unicode

| Przedział               | Kod                                                   |
| ----------------------- | ----------------------------------------------------- |
| 0x00 do 0x7F            | 0xxxxxxx                                              |
| 0x80 do 0x7FF           | 110xxxxx 10xxxxxx                                     |
| 0x800 do 0xFFFF         | 1110xxxx 10xxxxxx 10xxxxxx                            |
| 0x10000 do 0x1FFFFF     | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx                   |
| 0x200000 do 0x3FFFFFF   | 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx          |
| 0x4000000 do 0x7FFFFFFF | 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx |

### Zapis podanego ciągu

| Znak | Unicode | UTF-8                         | HEX      |
| ---- | ------- | ----------------------------- | -------- |
| P    | U+0050  | 0101 0000                     | 0x50     |
| r    | U+0072  | 0111 0010                     | 0x72     |
| o    | U+006F  | 0110 1111                     | 0x6F     |
| s    | U+0073  | 0111 0011                     | 0x73     |
| z    | U+007A  | 0111 1010                     | 0x7A     |
| ę    | U+0119  | 1100 0100 1001 1001           | 0xC499   |
|      | U+0020  | 0010 0000                     | 0x20     |
| z    | U+007A  | 0111 1010                     | 0x7A     |
| a    | U+0061  | 0110 0001                     | 0x61     |
| p    | U+0070  | 0111 0000                     | 0x70     |
| ł    | U+0142  | 1100 0101 1000 0010           | 0xC582   |
| a    | U+0061  | 0110 0001                     | 0x61     |
| c    | U+0063  | 0110 0011                     | 0x63     |
| i    | U+0069  | 0110 1001                     | 0x69     |
| ć    | U+0107  | 1100 0100 1000 0111           | 0xC487   |
|      | U+0020  | 0010 0000                     | 0x20     |
| 5    | U+0035  | 0011 0101                     | 0x35     |
| €    | U+20AC  | 1110 0010 1000 0010 1010 1100 | 0xE282AC |
| !    | U+0021  | 0010 0001                     | 0x21     |
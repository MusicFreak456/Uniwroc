# Zadanie 6

Dla przypomnienia
```c=
struct A
{
    int8_t a; // 1B, pozycja p
    // [3B padding], bo void* musi być na wielokrotności 4
    void *b; // 4B, pozycja p+4
    int8_t c; // 1B, pozycja p+8
    // [1B padding], bo int16, busi być na wielokrotności 2
    int16_t d; // 2B, pozycja p+10
    // Łącznie 12B, brak paddingu na końcu, bo to wielokrotność 4
};
```
### Kod w C
```c=
vs->d = us[1].a + us[j].c
```
### Kod trójkowy
```c=
t1 := us + 12
t2 := *t1
t3 := j * 12
t3 := t3 + 8
t4 := us + t3
t5 := *t4
t7 := vs + 10
*t7 := t2 + t5
```
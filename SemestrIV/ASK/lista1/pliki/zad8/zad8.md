## Zadanie 8

### Wstęp

Wyobraźmy sobie, że chcemy skopiować w pętli niepustą tablicę, której liczba elementów jest podzielna przez 8.

#### Najprostsze podejście

```c=
void simple(uint8_t *to, uint8_t* from, size_t count) {
    do {
        *to++ = *from++;
    } while(--count > 0);
}
```

Możemy jednak skorzystać z tego, że liczba elementów jest podzielna przez 8 i wykonywać po 8 kopiowań w jednym obrocie pętli, wtedy zaoszczędzimy wielokrotne, niepotrzebne sprawdzanie warunku.

#### Lepsze podejście

```c=
void better(uint8_t *to, uint8_t* from, size_t count) {
    int n = count / 8;
    do {
        *to++ = *from++;
        *to++ = *from++;
        *to++ = *from++;
        *to++ = *from++;
        *to++ = *from++;
        *to++ = *from++;
        *to++ = *from++;
        *to++ = *from++;
    } while(--n > 0);
}
```

Jak uogólnić takie rozwinięcie pętli na dowolną liczbę elementów tablicy? Wykonać najpierw modulo 8 takich operacji, a później analogicznie do powyższego sposobu, już dla podzielnej przez 8 liczby elementów.

#### Podejście z zadania

```c=
void secret(uint8_t *to, uint8_t* from, size_t count)
{
    size_t n = (count+7) / 8;
    switch(count%8)
    {
       case 0: do { *to++ = *from++;
       case 7:      *to++ = *from++;
       case 6:      *to++ = *from++;
       case 5:      *to++ = *from++;
       case 4:      *to++ = *from++;
       case 3:      *to++ = *from++;
       case 2:      *to++ = *from++;
       case 1:      *to++ = *from++;
                  } while (--n>0);
    }
}
```

### Zapis za pomocą goto

```c=
void secret_goto(uint8_t *to, uint8_t* from, size_t count)
{
    static void* labels[] = { &&label0, &&label1, &&label2, &&label3, &&label4, &&label5, &&label6, &&label7 };
    size_t n = (count + 7) / 8;

    goto *labels[count % 8];

    label0:      *to++ = *from++;
    label7:      *to++ = *from++;
    label6:      *to++ = *from++;
    label5:      *to++ = *from++;
    label4:      *to++ = *from++;
    label3:      *to++ = *from++;
    label2:      *to++ = *from++;
    label1:      *to++ = *from++;

    if(--n > 0) goto label0;
}
```
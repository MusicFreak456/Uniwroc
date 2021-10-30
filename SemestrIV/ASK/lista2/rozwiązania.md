# ASK -- Lista 2

###### tags: `ASK`

https://www.youtube.com/watch?v=7YvAYIJSSZY&ab_channel=RockwellVEVO

## Zadanie 1

```c=
(x > 0) || (x - 1 < 0)
```
Wyrażnie zwraca **fałsz** dla ```x = INT32_MIN```. Prawa strona alternatywy zwróci fałsz ponieważ ```INT32_MIN - 1 = INT32_MAX```, a to jest większe od 0.

```c=
(x & 7) != 7 || (x << 29 < 0)
```

Żeby lewa strona była fałszywa `x` musi mieć same jedynki na trzech najmłodszych bitach, ale wtedy przesunięty w prawo staje się liczbą ujemną, czyli wyrażenie zawsze zwróci **prawdę**.

```c=
(x * x) >= 0 
```

Działanie zwróci **fałsz** np. kiedy wynik mnożenia dwóch dodatnich liczb będzie mieć jedynkę na MSB. Np. zauważmy że $C_{16} \times C_{16} = 90_{16} = 10010000_{2}$. Zatem $C000_{16} \times C000_{16} = 90000000_{16}$, a to jest ujemna liczba w zapisie uzupełnień do 2.

```c=
x < 0 || -x <= 0
```

To działanie zawsze zwróci **prawdę**, ponieważ albo liczba będzie ujemna i lewa strona zostanie spełniona, albo będzie dodatnia i prawa będzie, gdyż każda liczba przeciwna liczby nieujemnej może zostać poprawnie zaprezentowana w kodzie uzupełnień do 2.

```c=
x > 0 || -x >= 0
```

Wyrażenie to zwróci **fałsz** dla `x = INT32_MIN`, ponieważ `INT32_MIN = -INT32_MIN` w kodzie uzupełnień do 2, czyli zarówno `x < 0` jak i `-x < 0`.

```c=
(x | -x) >> 31 == -1
```

Oczywiście dla `x=0` wyrażenie zwróci **fałsz** bo `0 >> 31` to dalej 0, a $0 \neq -1$.

```c=
x + y == (uint32_t)y + (uint32_t)x
```

Jeśli w pojedynczym wyrażeniu występują zmienne `unsigned` i `signed` to wartości ze znakiem są niejawnie rzutowane na wartości bez znaku, zatem to wyrażenie będzie zawsze obliczać się do **prawdy**.

```c=
x * ~y + (uint32_t)y * (uint32_t)x == -x
```

Jeśli w pojedynczym wyrażeniu występują zmienne `unsigned` i `signed` to wartości ze znakiem są niejawnie rzutowane na wartości bez znaku. Rozpiszmy zatem tą równość, skorzystamy z tego, że `-y = ~y + 1`, czyli `~y = -y - 1`.

```c=
x * ~y + y * x == -x
x * (-y - 1) + y * x == -x
- y*x -x + y*x == -x
-x == -x
```

## Zadanie 2

```c=
x = x ^ y
y = x ^ y
x = x ^ y
```
Najpierw w zmiennej `x` przechowujemy informację, na którym bicie `x` i `y` się różnią. Później zmieniamy te bity, na których się różniły.

## Zadanie 3

**overflow** - występuje kiedy wynik działania jest zbyt duży (na minusie lub na plusie) w stosunku do tego na co pozwala reprezentacja.

**underflow** - kiedy wynik jest co do modułu zbyt mały żeby mógł być zareprezentowany. Odnosi się głównie do liczb zmiennoprzecinkowych.

Nadmiar w dodawaniu może wystąpić tylko kiedy liczby są tego samego znaku. Można łatwo sprawdzić też, że wtedy wynik jest innego znaku.

```c=
((x + y ^ x)  & (x + y ^ y)) >> N - 1;
```

## Zadanie 4

### Dodawanie

Najpierw dodajemy dolne siedem bitów z każdej ósemki, nie spowoduje to przeniesienia. Następnie sprawdzamy XORem jaki bit znalazłby się na najbardziej znaczącym bicie każdej ósemki przy normalnym dodawaniu, i również XORem poprawiamy go w wyniku poprzedniego działania jeśli powinno wystąpić z nim przeniesienie.

```c=
result = (x & 0x7F7F7F7F) + (y & 0x7F7F7F7F);
result = result ^ ((x ^ y) & 0x80808080)
```

### Odejmowanie

Bardzo podobnie. Na początku ustawiamy MSB każdej ósemki bitów na 1, żeby nie było zapożyczeń i wykonujemy odejmowanie. Reszta tak samo jak w dodawaniu.

```c=
result = (x | 0x80808080) - (y & 0x7F7F7F7F);
result = ~(result ^ ((x ^ y) | 0x7F7F7F7F))
```

## Zadanie 5

Chcemy policzyć $x \cdot 3 / 4$.
$$
	\frac{3x}{4} = \frac{(2x + x)}{4} = \frac{x}{2} + \frac{x}{4}
$$
Możemy to zapisać w kodzie jako `x >> 1 + x >> 2`, ale może wystąpić sytuacja, że lewa i prawa strona dodawania będzie mieć bezpośrednio po przecinku 1, wtedy musimy dodatkowo 1 do całości.

```c=
(x >> 1) + (x >> 2) + x & (x >> 1) & 1
```

## Zadanie 6

Jeśli `x < y`, to `x - y < 0`. Możemy zbadać znak tego odejmowania bez powodowania przepełnienia badając znak wyrażenia `x/2 - y/2`. W wyniku odejmowania bitów, które pominęliśmy mogło jednak wystąpić przeniesienie kiedy najmniej znaczący bit x'a był zeram, a y'ka był jedynką. Musimy to zatem korygować odejmując wyrażenie `~x & y & 1`.

```c=
((x >> 1) - (y >> 1) - (~x & y & 1)) >> N-1
```

## Zadanie 7

Skorzystamy z własności podanej w treści, tj. `b ? x : y` można tłumaczyć do `b * x + !b * y`. Nie możemy jednak korzystać z mnożenia ani zaprzeczenia, więc musimy znaleźć inny sposób.
Jeśli warunek `b` będzie przyjmował wartości -1 lub 0, to wtedy możemy zapisać if'a w zależności od wartości b w następujący sposób
```c=
b & x + ~b & y
```
Korzystając z własności shiftowania w prawo, czyli że jeśli na MSB był jedynka, to dokładamy z lewej strony jedynkę, możemy zapisać funkcję abs jako
```c=
((x >> N-1) & -x) + (~(x >> N-1) & x) 
```

## Zadanie 8

```c=
-(x >> N-1 & 1) | -x >> N-1 & 1;
```

* Jeśli liczba jest ujemna to chcemy zwrócić -1, czyli -MSB. Prawa strona alternatywy bitowej wtedy jest równa 1 (dla INTN_MIN), lub 0, ale nie wpływa to na wynik, gdzyż -1 to same jedynki w kodzie uzupełnień do 2.
* Jeśli liczba jest dodatnia to lewa strona alternatywy to 0, bo MSB x to 0. Liczba przeciwna do x musi mieć MSB = 1 (działa, bo -INTN_MAX, mieści się w intN_t). Czyli prawą stronę możemy ustawić na MSB(-x).
* Jeśli jest zerem to obie strony są równe zero, więc alternatywa bitowa zwróci 0.

## Zadanie 9

Można zastosować metodę dziel i zwyciężaj. Mając dwa bity możemy określić parzystość jedynek wśród nich za pomocą xora. Znając parzystość jedynek w dwóch przedziałach, parzystość ich obu będzie również określona przez xor tych wartości. Wynik xora będziemy zapisywać zawsze w LSB każdej dwójki, czwórki, ósemki,... itd. bitów.


```c=
int32_t odd_ones(uint32_t x){
	x = x ^ (x>>1);
	x = x ^ (x>>2);
	x = x ^ (x>>4);
	x = x ^ (x>>8);
	return (x ^ (x>>16)) & 1;
}
```
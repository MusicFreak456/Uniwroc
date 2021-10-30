# Zadanie 5

## Przykład pierwszy
### Kod w C
```c=
s += b[j+1] + b[--j];
```
### Kod trójkowy
```c=
t1 := j + 1
t2 := t1 * 4
t3 := b[t2]
s  := s + t3
t4 := j - 1
t5 := t4 * 4
t6 := b[t4]
s  := s + t6
```

## Przykład drugi

### Kod w C
```c=
a[i++] -= *b * (c[j*2] + 1);
```
### Kod trójkowy
```c=
t1 := i * 4
t2 :=  a[t1]
t2 := *b
t3 := j * 2
t4 := t3 * 4
t5 := c[t4]
t6 := t5 + 1
t7 := t2 * t6
a[t1] := t2 - t7
i := i + 1
```
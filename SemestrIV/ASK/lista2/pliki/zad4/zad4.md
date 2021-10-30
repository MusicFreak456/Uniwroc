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
result = (x | 0x80808080) + (y & 0x7F7F7F7F);
result = result ^ ((x ^ y) & 0x80808080)
```
## Zadanie 7

Skorzystamy z własności podanej w treści, tj. `b ? x : y` można tłumaczyć do `b * x + !b * y`. Nie możemy jednak korzystać z mnożenia ani zaprzeczenia, więc musimy znaleźć inny sposób.
Jeśli warunek `b` będzie przyjmował wartości -1 lub 0, to wtedy możemy zapisać if'a w zależności od wartości b w następujący sposób
```c=
b & x + ~b & x
```
W naszym zadaniu za `b` możemy przyjąć -MSB(x).
```c=
((-(x >> N-1)) & x) + (~(-(x >> N-1)) & -x) 
```
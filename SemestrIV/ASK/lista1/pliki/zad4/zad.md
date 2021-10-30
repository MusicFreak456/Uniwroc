## Zadanie 4

**volatile** - jest to modyfikator typu, który informuje kompilator o tym, że stan zmiennej może być zmieniany przez zewnętrzne programy. Czyli na przykład nie może zostać usunięta, lub zastąpiona w programie, który nigdzie nie modyfikuje jej stanu.

**static** (zmienne lokalne i procedury) - pozwala na utrwalenie wartości zmiennej pomiędzy wowołaniami procedury.

**static** (zmienne globalne) - ogranicza widoczność zmiennej wyłącznie do pliku w którym znajduje się jej deklaracja.

**restrict** - zastosowane go w stosunku do wskaźnika, zapewnia kompilator, że wskaźnik ten jest jedynym wskazującym na jego komórkę pamięci (i jej pochodne). Pozwala to na wykonanie przez kompilator pewnych optymalizacji, jednak odpowiedzialność za dopilnowanie prawdziwości tego założenia wciąż leży po stronie programisty.
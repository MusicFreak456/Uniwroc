---
marp: true
class: invert
theme: uncover
paginate: true
_paginate: false
---

###### Programowanie Funkcyjne 2021/2022
## **Projekt końcowy - pierwsza prezentacja**

#### Cezary Świtała

---

## **Temat projektu**

Jako cel swojego projektu obrałem implementację języka programowania *Prolog*, zrealizowaną w języku funkcyjnym OCaml.

---

## **Czym jest Prolog?**

---

## **Składnia**

Programy to listy klauzul. 
Klauzula to fakt lub reguła wnioskowania.


---

## Fakt

Element `X` należy do listy, która się od niego zaczyna.

```prolog
list_member(X, [X|_]).
```

---

## Reguła wnioskowania

Jeśli `X` należy do listy `Y`, to należy do listy `Y` rozszerzonej o 1 element:

```prolog
list_member(X, [_|Y]) :-
  list_member(X,Y).
```

Ciało reguły może być złożone.
`,` oznacza koniunkcję, a `;` alternatywę.

---

## **Sementyka**

Po załadowaniu programu możemy kierować do niego zapytania. Mają one postać klauzul (być może złożonych). 

*(Prezentacja na żywo)*

---

## **Co jest do zrobienia?**

Interaktywny *REPL* (ang. *read-eval-print loop*), w którym będzie można wprowadzać zapytania uruchamiające programy oraz odczytywać ich wyniki. Co za tym idzie konieczny będzie parser zapytań/bazy faktów, oraz ich interpreter.

---

## **Parser**

Parsowanie będzie odbywało się do drzewa składniowego (AST) i rozbite będzie na dwa moduły.

* Moduł leksera - zamienia strumienia znaków na strumień tokenów. Generowany przez narzędzie `ocamllex`.
* Moduł parsera - zawieraja definicję składni języka i zamieniaja ciąg tokenów na drzewo składniowe. Generowany przez narzędzie `menhir`.


---

## **Interpreter**

Implementację przetwarzania zapytań zamierzam oprzeć na idei opisanej w znanym z przedmiotu *Metody Programowania* podręczniku *Structure and Interpretation of Computer Programs* (*SICP*).

----

## **Przetwarzanie zapytań**

Idea przetwarzania zapytań będzie opierać się na strumnieniach *ramek*.

Ramka to "środowisko" przechowujące możliwe poprawne podstawienia termów za zmienne.

----

## **Zapytania proste**

Otrzymując zapytanie proste przeszukujemy bazę klauzul, próbując dopasować do nich zapytanie.

---

## **Dopasowanie**

Polega na znalezieniu takiego podstawienia zmiennych w zapytaniu, żeby odpowiadało ono nagłówkowi jakiejś klauzuli.

Udane dopasowanie generuje nową ramkę.

---

## **Dopasowanie - przykład**

W bazie znajduje się fakt `parent(julia, tomasz).`
Otrzymujemy zapytanie `parent(julia,X).`
Treść zapytania dopasuje się do powyższego faktu i wygeneruje ramkę, w której `X = tomasz`.

Dopasowanie to tak na prawdę problem *unifikacji*.

---

## **Zapytania proste - strumienie**

Żeby przygotować naszą implementację do przetwarzania zapytań złożonych i reguł, dodajemy strumienie ramek.

Dopasowanie będzie wykonywane względem ramek w strumieniu wejściowym, który inicjujemy z jedną pustą ramką.


---

<div>
  <img src="./simple.svg" width=1150>
</div>

---

## **Zapytania złożone**

Przetwarzanie zapytań złożonych sprowadza się teraz do odpowiedniego złożenia strumieni z zapytań prostych z których się ono składa.

---

## **Zapytania złożone - `A,B`**

<div>
  <img src="./complex_and.svg" width=1150>
</div>

---

## **Zapytania złożone - `A;B`**

<div>
  <img src="./complex_or.svg" width=1150>
</div>

---

## **Rozwiązywanie reguł**

Po dopasowaniu do nagłówka wykorzystujemy uzyskaną ramkę jako początkową ramkę do przetworzenia ciała reguły.

---

## **Podsumowanie**

#### Cel minimalny

Implementacja działająca dla podstawowych klauzul i zapytań.

#### Co dalej?

* Rozszerzenia języka (np. o arytmetykę)
* Szybkie przeszukiwanie bazy klauzul
* Optymalizacja algorytmu unifikacji

/*
 * UWAGA! W poniższym kodzie należy zawrzeć krótki opis metody rozwiązania
 *        zadania. Będzie on czytany przez sprawdzającego. Przed przystąpieniem
 *        do rozwiązywania zapoznaj się dokładnie z jego treścią. Poniżej należy
 *        wypełnić oświadczenie o samodzielnym wykonaniu zadania.
 *
 * Oświadczam, że zapoznałem(-am) się z regulaminem prowadzenia zajęć
 * i jestem świadomy(-a) konsekwencji niestosowania się do podanych tam zasad.
 *
 * Imię i nazwisko, numer indeksu: Cezary Świtała, 316746
 */

        .text
        .globl  cubef
        .type   cubef, @function

/*
 Opis rozwiązania: (odpowiednim punktom przyporządkowane są segmenty kodu poniżej)

          * 1. Wyciągamy znak liczby, wiemy że podniesienie liczby
          do sześcianu go nie zmieni, więc na  końcu  wpiszemy  go
          prosto w wynik.

          * 2. Za  pomocą  maski  0x7f800000  wyciągamy  wykładnik
          liczby i przesuwamy na początek liczby,  żeby móc używać
          go  wygodnie.  Następnie  odejmujemy  127,  czyli  bias.
          Od razu przemnażamy go przez 3, jako że 2^a + 2^a + 2^a=
          2^(3a).

          * 3. Podnosimy mantysę do trzeciej potęgi. Wynik pierws-
          zego mnożenia na pewno zmieści się w samym RAX'ie, gdyż
          są to 24 bitowe liczby. Wynik  drugiego  już  na  pewno
          rozleje  się  na  RDX, ale skoro  w praktyce mnożyliśmy
          liczby z przedziału [1,2), to wynik jest  z  przedziału
          [1,4), więc za przecinkiem,  który  teraz  jest  za  69
          bitem licząc od prawej, może się znajdować  co najwyżej 
          3 bitowa liczba całości.

          * 4. Sprawdzamy  o  ile  będziemy   musieli   przesunać 
          przecinek w lewo, przesuwając liczbę w prawo i testując
          czy jest równa zero.

          * 5. Skoro wynikowa mantysa będzie  mieć  24  bity,  to
          będzie zawierać wszystkie bity z górnej  połowy  wyniku
          mnożenia i od 16 do 18 bitów z  dolnej.  Przygotowujemy
          zatem górną połowę  do  bycia  złocząną  alternatywą  z 
          bitami z niższej  połowy.  Przesuwamy  ją   w   lewo  o
          (18 minus ewentualne przesunięcie) bitów.

          * 6. Jeśli przesuwamy przecinek w  lewo, musimy również
          zwiększyć wykładnik.

          * 7. Kolejnym   krokiem   będzie  wyznaczenie  wartości
          guard, round i sticky, żeby dokonać zaokrąglenia.
              * 7.1. Sprawdzamy czy wszystkie bity poniżej
              round są zerowe, jeśli nie to sticky = 1.
              * 7.2. Wyciągamy round.
              * 7.3. Wyciągamy guard  i od razu liczymy  (guard | 
              sticky).
          * 8. Zaokrąglamy w górę jeśli (guard | sticky)  & round,  
          więc wyliczamy tą wartość.  Łączymy  części  mantysy  i 
          zaokrąglamy.
          * 9. Na  skutek poprzedniej operacji mantysa  mogła  się
          znowu zdenormalizować, jeśli  miała  same  jedynki,  ale 
          wtedy   wystarczy   dodać  1  do  wykładnika.  Następnie 
          wycinamy wiodącą jednykę, która jest tam w domyśle.
          * 10. Przywracamy wykładnik do poprzedniej postaci.
          * 11. Jeśli wykładnik wyszedł większy niż dopuszczalny,
          lub ujemny, to zwracamy inf, lub 0.
          * 12. Dodajemy wcześniej wyznaczony znak.
 */

cubef:
  # 1  #####################
  movl $0x80000000, %esi
  andl %edi, %esi
  # ########################
  
  # 2  #####################
  movl $0x7f800000, %r8d
  andl %edi, %r8d
  shrl $23, %r8d
  subl $127, %r8d 
  leaq 0x0(%r8,%r8,2), %r8
  # ########################

  # 3  #####################
  andl $0x00ffffff, %edi
  orl  $0x00800000, %edi
  movl %edi, %eax
  mulq %rdi
  mulq %rdi
  # ########################

  # 4  #####################
  movq %rdx, %r9
  shrq $6, %r9
  xorq  %rcx, %rcx

  movq $1, %r10
  cmpq $1, %r9
  cmovgeq %r10, %rcx

  addq $1, %r10
  cmpq $2, %r9
  cmovgeq %r10, %rcx
  # ########################

  # 5  #####################
  movq %rcx, %r9
  negq %rcx
  addq $18, %rcx
  shlq %cl, %rdx
  movq %r9, %rcx
  # ########################

  # 6  #####################
  addq %rcx, %r8
  # ########################

  # 7  #####################

    # 7.1 #####################
    movq $0x00001C0000000000, %r10
    movq $0x00001fffffffffff, %r9
    shlq %cl, %r10
    orq  %r9, %r10
    andq %rax, %r10
    test %r10, %r10
    movl $1, %r11d
    cmovnzq %r11, %r10
    # ########################

    # 7.2 #####################
    shrq $32, %rax
    addl $13, %ecx
    shrq %cl, %rax
    andq %rax, %r11
    # ########################

    # 7.3 #####################
    shrq $1, %rax
    orq  %rax, %r10
    # ########################

  # ########################

  # 8  #####################
  andq %r11, %r10
  orq %rdx, %rax
  addl %r10d, %eax
  # ########################

  # 9  #####################
  movl %eax, %r9d
  shrl $24, %r9d
  addl %r9d, %r8d
  andl $0x007fffff, %eax
  # ########################

  # 10 #####################
  addl $127, %r8d
  movq %r8, %rcx
  shll $23, %r8d
  orl %r8d, %eax
  # ########################

  # 11 #####################
  movl $0x7f800000, %r9d
  movl $0, %r10d
  cmpl $0, %ecx
  cmovngq %r10, %rax
  cmpl $255, %ecx
  cmovnlq %r9, %rax
  # ########################

  # 12 #####################
  orl  %esi, %eax
  # ########################

  ret

  .size   cubef, .-cubef

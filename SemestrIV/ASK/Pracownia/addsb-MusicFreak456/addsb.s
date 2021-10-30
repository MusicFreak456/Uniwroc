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
        .globl  addsb
        .type   addsb, @function

/*
Części rozwiązania wzorowane są na rozwiązaniach zadań 3 i 4 z listy ćwiczeniowej
nr. 2.

Opis rozwiązania: (odpowiednim punktom przyporządkowane są segmenty kodu poniżej)

        * 1. Zerujemy górne bity w każdej ósemce i dodajemy wektory
             do siebie, zerowanie zapobiegnie propagacji przeniesienia.
                   result = (x & 0x7F7F...7F7F) + (y & 0x7F7F...7F7F)
        * 2. Sprawdzamy xorem jakie bity znalazłyby się na najstarszym
             bicie każdej ósemki przy normalnym dodawaniu i również xorem 
             dodajemy je do wyniku porzedniego działania.
                        result ^= ((x ^ y) & 0x8080...8080)
        * 3. Następnie musimy wyznaczyć, w której ósemce wystąpił overflow.
             Przypominam, że występuje on kiedy dwa składniki są tego samego
             znaku (~(x^y)), a wynik jest innego (x ^ result). Możemy o to 
             zapytać wyrażeniem
                    (~(x ^ y) & (x ^ result)) & 0x8080...8080
        * 4. Teraz każdy górny bit w ósemce pokazuje czy będzie ona nasycana
             w wyniku. Rozsmarowujemy go po całej ósemce, żeby dostać maskę
             mówiącą które ósemki są do podmiany. Możemy do tego użyć maski
             0x7F7F...7F7F, którą już mamy w rejestrze r10. Dodajemy do niej
             maskę mówiącą czy są overflowy przsuniętą o 7 w prawo, wtedy
             jeśli overflow był to 7F przejdzie na 80, a to xorowane z 7F
             da FF, wpp będzie xor 7F z 7F, czyli 00.
        * 5. Wycinamy za pomocą dopełnienia bitowego tej maski, te ósemki 
             z dotychczasowego wyniku, które muszą zostać zamienione.
        * 6. Jeśli w takiej ósemce znak wyniku jest dodatni, czyli argumentów 
             ujemny, to musimy wstawić INT8_MIN, wpp. INT8_MAX, czyli w praktyce
             chcemy w tamte miejsca wstawić 0x7F + znak_agurmentu. Wyciągamy
             zatem znaki dowolnego arguementu.
                              signs = x & 0x8080808080808080
             Dodajemy znak bitu w każdej ósemce do 0x7F.
                                     signs >>= 7
                                 0x7F7F...7F7F + signs
             Następnie wycinamy tylko te 8 bitów które potrzebujemy za pomocą maski
             utworzonej wcześniej i wstawiamy je do wyniku alternatywą.
 */
 addsb:
        # 1  #####################
        movq    $0x7F7F7F7F7F7F7F7F, %r9
        movq    %rdi, %rdx
        movq    %rsi, %rcx
        andq    %r9,  %rdx
        andq    %r9,  %rcx
        addq    %rdx, %rcx
        # 2 #######################
        movq    $0x8080808080808080, %r8
        xorq    %rdi, %rsi
        andq    %r8, %rsi
        xorq    %rsi, %rcx
        # 3 #######################
        movq    %rdi, %rax
        xorq    %rcx, %rax
        notq    %rsi
        andq    %rsi, %rax
        andq    %r8, %rax
        # # 4 #######################
        shrq    $7, %rax
        addq    %r9, %rax
        xorq    %r9, %rax
        # 5 #######################
        movq    %rax, %rsi
        notq    %rax
        andq    %rcx, %rax 
        # 6  #######################
        andq    %r8, %rdi
        shrq    $7, %rdi
        addq    %rdi, %r9
        andq    %rsi, %r9
        orq     %r9, %rax
        ret

        .size   addsb, .-addsb

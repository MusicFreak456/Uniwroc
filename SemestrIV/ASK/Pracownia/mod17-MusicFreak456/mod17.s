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
    .data
        mask1:
            .quad 0x0F0F0F0F0F0F0F0F
        mask2:
            .quad 0xF0F0F0F0F0F0F0F0
        mask3:
            .quad 0x00FF00FF00FF00FF
        mask4:
            .quad 0x0FF00FF00FF00FF0
        num1:
            .quad 0x88
        num2:
            .quad 0x11
    .text
    .globl  mod17
    .type   mod17, @function

/*
 Opis rozwiązania: (odpowiednim punktom przyporządkowane są segmenty kodu poniżej)

        * 1. Wycinamy za pomocą maski cyfry na parzystych  i 
        nieparzystych    pozycjach  z  liczby   (w   zapisie  
        szesnastkowym są one równoważne liczbom na czwórkach 
        bitów tej liczby).

        * 2. Będziemy teraz sumować  cyfry  metodą  dziel  i
        zwyciężaj. Skorzystamy z obserwacji, że  maksymalnie
        z parzystych lub nieparzystych uzyskamy 8 * 15 = 120 
        a to mieści  się  na  jednym  bajcie.  Możemy  zatem
        liczyć ich sumę jednocześnie wewnątrz jednej  liczby
        (nazwanej  w    komentarzach     'result'),       na 
        parzystych    i   nieparzystycj   bajtach   w   celu
        zaoszczędzenia instrukcji. Wynik  dla  nieparzystych 
        znajdzie się  na   na   najniższym   bajcie,  a  dla
        parzystych na drugim od prawej.

        * 3. Odejmujemy  sumę  parzystych  od nieparzystych.

        * 4. Wynik  będzie  z  przedziału  [-120,120],   co   
        może  wydawać  się   problematyczne,   bo   tracimy  
        informację o cyfrach w przypadku ujemnym, ale można 
        to   łatwo   naprawić   dodając   odpowiednio  dużą 
        wielokrotność  siedemnastki,  tak  żeby  wynik  był 
        dodatni (nie zmienia to reszty z dzielenia).

        * 5. Po dodaniu 136 (wielokrotności 17) kiedy wynik
        był ujemny, otrzymujemy teraz liczbę  z  przedziału
        [0,135]. Jej  zapis  ma  co  najwyżej  dwie  cyfry.
        Odejmujemy pierwszą od drugiej.

        * 6. Dostaniemy   coś   z   przedziału    [-15,15], 
        wystarczy  zastosować  ten  sam  trick  z  dodaniem
        wielokrotności    17   (tym razem po prostu 17)   i 
        otrzymamy liczbę z przedziału [0, 16], którą możemy
        po prostu zwrócić  gdyż  sama   jest   sobie  równa  
        reszcie z dzielenia przez 17.
 */

mod17:
    # 1  #####################
    movq    %rdi,%rax
    andq    (mask1), %rax # odd
    andq    (mask2), %rdi # even
    # ########################

    # 2  #####################
    # odd = (odd + (odd >> 8)) & 0x00FF00FF00FF00FF;
    movq    %rax, %r9
    shrq    $8, %r9
    addq    %r9, %rax
    andq    (mask3), %rax

    # even = (even + (even >> 8)) & 0x0FF00FF00FF00FF0
    movq    %rdi, %r11
    shrq    $8, %r11
    addq    %r11, %rdi
    andq    (mask4), %rdi

    # uint64_t result = odd | (even << 4);
    shlq    $4, %rdi
    orq     %rdi, %rax

    # result = result + (result >> 16);
    movq    %rax, %r11
    shrq    $16, %r11
    addq    %r11, %rax

    # result = result + (result >> 32);
    movq    %rax, %r11
    shrq    $32, %r11
    addq    %r11, %rax
    # ########################

    # 3  #####################
    # result = (result & 0xFF) - ((result & 0xFF00) >> 8);
    movq    %rax, %r11
    andq    $0xff, %rax
    andq    $0xff00, %r11
    shrq    $8, %r11
    xorq    %rdx, %rdx
    subq    %r11, %rax
    # ########################

    # 4  #####################
    ## if(result < 0) result += 136;
    cmovlq  (num1), %rdx
    addq    %rdx, %rax
    # ########################

    # 5  #####################
    # result = (result & 0xF) - (result >> 4);
    movq    %rax, %r9
    andq    $0xf, %rax
    shrq    $4, %r9
    xorq    %rdx, %rdx
    subq    %r9, %rax
    # ########################

    # 6  #####################
    ## if(result < 0) result += 17;
    cmovlq  (num2), %rdx
    addq    %rdx, %rax
    # ########################

    ret

    .size   mod17, .-mod17

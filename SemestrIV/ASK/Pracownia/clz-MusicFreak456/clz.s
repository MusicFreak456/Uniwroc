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
        .globl  clz
        .type   clz, @function

/*
Opis rozwiązania: (odpowiednim punktom przyporządkowane są segmenty kodu poniżej)
        * 1. Rozsmarowujemy górny bit na całą resztę liczby
             żeby pozbyć się wszystkich zer, które nie są
             wiodące. Robimy to w czasie logarytmicznym
             najpierw rozmarowując go na 2 bity, później
             te 2 na 4 itd.
        * 2. Po pierwszym kroku  wiemy,  że jedyne zera w 
             naszej liczbie to te  wiodące.  Stosujemy na
             niej   negację   logiczną,  żeby  sprowadzić
             problem policzenia wiodących zer do problemu
             policzenia zapalonych bitów w liczbie.
        * 3. Stosujemy algorytm popcount, pokazany choćby
             na ćwiczeniach, ale  z dodatkową  obserwacją,  
             że w momencie kiedy  mamy  zapisaną   liczbę 
             zapalonych bitów dla czwórek bitów w  bajcie,
             to nie musimy już stosować masek do wyciągania
             kolejnych paczek bitów, gdyż największa możliwa
             wartość -- 64, mieści się na 8 bitach, zatem
             dodawanie przesuniętych liczb, nie spowoduje
             od tego momentu żadnych przeniesień do wyższych
             bajtów, wystarczy zatem ustalić że wynik dla
             ósemek bitów, szesnastek  bitów,  połówek   i 
             całości liczby będziemy trzymać w najmłodszym
             bajcie liczby.
        * 4. Zwracamy najmłodszy bajt jako wynik.

Algorytm pocount w tej implementacji również ma złożoność
O(log n), więc wykonujemy dwa razy coś co jest O(log n),
czyli złożoność całego algorytmu to również O(log n).
 */

clz:
        # 1 ################## <--- Tym blokom odpowiadają opisy powyżej
        movq     %rdi,%rax
        shrq     %rdi
        orq      %rdi, %rax
        movq     %rax, %rdi
        shrq     $2, %rax
        orq      %rdi, %rax
        movq     %rax, %rdi
        shrq     $4, %rax
        orq      %rdi, %rax
        movq     %rax, %rdi
        shrq     $8, %rax
        orq      %rdi, %rax
        movq     %rax, %rdi
        shrq     $16, %rax
        orq      %rdi, %rax
        movq     %rax, %rdi
        shrq     $32, %rax
        orq      %rdi, %rax
        # 2 ##################
        notq     %rax
        # 3 ##################
        movq     %rax, %rdi
        movq     $0x5555555555555555, %r8
        shrq     %rax
        andq     %r8, %rdi
        andq     %r8, %rax
        addq     %rdi, %rax
        movq     %rax, %rdi
        movq     $0x3333333333333333, %r9
        shrq     $2, %rax
        andq     %r9, %rdi
        andq     %r9, %rax
        addq     %rdi, %rax
        movq     %rax, %rdi
        movq     $0x0F0F0F0F0F0F0F0F, %r10
        shrq     $4, %rax
        andq     %r10, %rdi
        andq     %r10, %rax
        addq     %rdi, %rax
        movq     %rax, %rdi
        shrq     $8, %rax
        addq     %rdi, %rax
        movq     %rax, %rdi
        shrq     $16, %rax
        addq     %rdi, %rax
        movq     %rax, %rdi
        shrq     $32, %rax
        addq     %rdi, %rax
        # 4 ##################
        andq     $0x00000000000000FF, %rax
        ret

        .size   clz, .-clz

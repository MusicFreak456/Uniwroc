        .globl  puzzle
        
        .text
puzzle:
        push %rbp              # odkładamy %rbp na stos 
        xorl %eax, %eax        # zerujemy result
        mov %rsi, %rbp         # zachowujemy *p
        push %rbx              # odkładamy %rbx na stos
        mov %rdi, %rbx         # zachowujemy wartość n
        sub $24, %rsp          # rezerwujemy 24 (3 razy 8) bajty
        test %rdi, %rdi        # SF=MSB(n&n), OF=0, jeśli n=0 to ZF=1
        jle .L1                # Skocz jeśli n<=0 (ZF=1 lub SF!=OF)
        lea 8(%rsp), %rsi      # Zapisujemy w rsi adres drugiej zmiennej 
        lea (%rdi,%rdi), %rdi  # nowe n = n + n (n << 1)
        call puzzle            # wywołujemy n z parametrami n << 1, &tmp
        add 8(%rsp), %rax      # result += tmp
        add %rax, %rbx         # n += tmp
.L1:    mov %rbx, (%rbp)       # *p = n
        add $24, %rsp          # zwalniamy zarezerwowane miejsce na stosie
        pop %rbx               # przywracamy wartość rbx
        pop %rbp               # i rbp
        ret
		
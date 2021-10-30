#include<stdint.h>
#include<limits.h>
#include<stdio.h>
#include<assert.h>

const int N = 16;

void binary_print(int16_t x){
    for (int i = 15; i>=0; i--)
    {
        printf("%d", (int)  (x & (1 << i)) >> i );
    }
    printf("\n");
}

int16_t int2float(int16_t i){
    int16_t B = 15;
    int16_t M = 11;
    int16_t s = i & 0x8000;
    int16_t abs = ((i >> N-1) & -i) + (~(i >> N-1) & i); 
    int16_t one_pos = (N - (__builtin_clz(abs) - 16)) - 1;
    int16_t shift = (M - one_pos) - 1;
    int16_t exp = one_pos;
    int16_t E = (exp + B) << M-1;
    int16_t man = abs << shift;

    // printf("s = %hd\n", (short int)(s >> N-1));
    // printf("abs = %hd\n", (short int)abs);
    // binary_print(abs);
    // printf("l_one_pos = %d\n", (short int)one_pos);
    // printf("shift = %hd\n", (short int)shift);
    // binary_print(abs << shift);
    printf("exp = %hd\n", (short int)exp);
    binary_print(exp + B);

    return s | (E & 0x7C00) | (man & 0x03FF);
}

int main() {
    // int2float(-2);
    binary_print(int2float(-2));
}
// Cezary Świtała
// 316746
// KLO

#include<stdio.h>

const int N = 2021;

int main() {
    long unsigned int a;
    long unsigned int b;

    scanf("%lu %lu", &a, &b);

    long unsigned int i = ( a%N ? (a / N + 1): a /N) * N;

    for(; i<=b; i+=N){
        if( i+N <= b)
         printf("%lu ", i);
        else
         printf("%lu", i);
    }

    return 0;
}
#include<stdint.h>
#include<assert.h>
#include<stdbool.h>
#include<limits.h>
#include<stdio.h>

const int N = 32;

int func(int32_t x){
    return (x >> 1) + (x >> 2);
}

int main() {

    printf("%d\n", func(-4));

    return 0;
}
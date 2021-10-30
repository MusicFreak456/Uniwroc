#include<stdint.h>
#include<limits.h>
#include<stdio.h>


int main() {

    volatile int32_t x = INT32_MIN;
    volatile int32_t y = 1;

    printf("%d\n", ((int32_t)(x-1)) < 0);

    int32_t z = x - 1;

    printf("%d\n", z < 0);

    return 0;
}
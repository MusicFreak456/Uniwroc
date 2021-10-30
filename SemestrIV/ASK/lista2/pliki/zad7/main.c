#include<stdint.h>
#include<assert.h>
#include<stdbool.h>
#include<limits.h>
#include<stdio.h>

const int N = 32;

int32_t absolute(int32_t x){
    int32_t b = x >> N-1;
    return (-b) & x + ~(-b) & -x;
}

int main() {

    // assert(absolute(5) == 5);
    printf("%d", absolute(10));

    return 0;
}
#include<stdint.h>
#include<assert.h>
#include<stdbool.h>
#include<limits.h>
#include<stdio.h>

const int N = 32;

bool comparison(int32_t x, int32_t y){
    return ((x >> 1) - (y >> 1) - (~x & y & 1)) >> N-1;
}

int main() {

    assert(comparison(-5,6));
    assert(comparison(-3,10));
    assert(!comparison(11,9));
    assert(comparison(-20,-10));

    return 0;
}
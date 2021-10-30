#include<stdint.h>
#include<limits.h>
#include<stdio.h>
#include<assert.h>

const int N = 16;

int16_t lesser(int16_t x, int16_t y){
    return ((x & 0x7FFF) >> 1) - ((y & 0x7FFF) >> 1) - (~x & y & 1);
}

int16_t lesser_float16(int16_t x, int16_t y){
    return ( (
        ( (x ^ y) & x ) |
        ( x & y & lesser(y,x) ) |
        ( ~(x & y) & lesser(x,y) )
    ) >> (N-1) ) & ( (x|y) != 0x80000000 );
}

int main() {
    assert(lesser(2,3)>>(N-1));
    assert(!(lesser(3,2)>>(N-1)));
    assert(!(lesser(3,3)>>(N-1)));

    int16_t x = 0b0011011001100000;
    int16_t y = 0b0011010110000000;
    int16_t z = 0b1011010110000000;

    assert(!lesser_float16(x,y));
    assert(lesser_float16(y,x));
    assert(lesser_float16(z,x));
    assert(lesser_float16(z,y));
}
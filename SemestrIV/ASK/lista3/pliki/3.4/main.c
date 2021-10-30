#include<stdint.h>
#include<limits.h>
#include<stdio.h>
#include<assert.h>

int main() {

    volatile int32_t x = 6 ;
    volatile int32_t y = 8388611;
    volatile int32_t z = 1073741825;
    volatile double dx = (double)x;
    volatile double dy = (double)y;
    volatile double dz = (double)z;

    printf("%lf\n", dx * dy);

    assert( (dx * dy) * dz == dx * (dy * dz));

}
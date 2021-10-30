#include<stdint.h>
#include<assert.h>
#include<stddef.h>

uint32_t popcount(uint32_t x) {
    x = (0x55555555 & x) + ((x >> 1) & 0x55555555);
    x = (0x33333333 & x) + ((x >> 2) & 0x33333333);
    x = (0x0F0F0F0F & x) + ((x >> 4) & 0x0F0F0F0F);
    x = (0x00FF00FF & x) + ((x >> 8) & 0x00FF00FF);
    return (0x0000FFFF & x) + ((x >> 16) & 0x0000FFFF);
}

int main() {

    size_t x;

    assert(popcount(4) == 1);
    assert(popcount(5) == 2);
    assert(popcount(7) == 3);
    assert(popcount(0x30000003) == 4);

    return 0;
}
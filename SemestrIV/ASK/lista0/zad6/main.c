#include<stdint.h>
#include<assert.h>

/*
    Big endian - najbardziej znaczący bajt jest umieszczony na początku
    Little endian - na odwrót
*/

uint32_t little_to_big_endian(uint32_t x) {
    return
        (x >> 24) |
        ((x & 0x00FF0000) >> 8) |
        ((x & 0x0000FF00) << 8) |
        (x << 24);
}

int main() {
    
    assert(little_to_big_endian(0xAABBCCDD) == 0xDDCCBBAA);

    return 0;
}
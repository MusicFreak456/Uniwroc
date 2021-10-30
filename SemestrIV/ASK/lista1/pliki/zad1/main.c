#include<stdint.h>
#include<assert.h>

uint32_t copy_bit(uint32_t x, uint32_t i, uint32_t k) {
    return (x & ~(1 << k)) | ( ((1 << i) & x) << (k - i) );
}

int main() {

    assert(copy_bit(3, 0, 2) == 7);
    assert(copy_bit(7, 2, 3) == 15);

    return 0;
}
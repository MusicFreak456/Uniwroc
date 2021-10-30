#include<stdint.h>
#include<stdio.h>
#include<assert.h>

uint32_t multiply_by_pow_of_two(uint32_t x, uint32_t y) {
    return x << y;
}

uint32_t divide_by_pow_of_two_floor(uint32_t x, uint32_t y) {
    return x >> y;
}

uint32_t mod_pow_of_two(uint32_t x, uint32_t y) {
    return x & ((1 << y) - 1);
}

uint32_t divide_by_pow_of_two_ceil(uint32_t x, uint32_t y) {
    return (x + ((1 << y) - 1)) >> y;
}

int main() {
    
    assert(multiply_by_pow_of_two(1,2) == 4);
    assert(multiply_by_pow_of_two(2,2) == 8);

    assert(divide_by_pow_of_two_floor(4,2) == 1);
    assert(divide_by_pow_of_two_floor(5,1) == 2);

    assert(mod_pow_of_two(5,2) == 1);
    assert(mod_pow_of_two(15,2) == 3);
    assert(mod_pow_of_two(16,2) == 0);

    assert(divide_by_pow_of_two_ceil(4,2) == 1);
    assert(divide_by_pow_of_two_ceil(5,1) == 3);

    return 0;
}
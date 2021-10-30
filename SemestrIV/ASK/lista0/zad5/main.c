#include<stdint.h>
#include<stdio.h>
#include<assert.h>
#include<stdbool.h>

bool is_pow_of_two(uint32_t x) {
    return !(x == 0) && !(x & (x - 1));
}

int main() {
    
    assert(!is_pow_of_two(10));
    assert(is_pow_of_two(8));
    assert(is_pow_of_two(1));
    assert(!is_pow_of_two(0));

    return 0;
}
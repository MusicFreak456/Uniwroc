#include<stdint.h>
#include<assert.h>

uint32_t set_kth_to_zero(uint32_t x, uint32_t k) {
    return x & ~(1 << k);
}

uint32_t set_kth_to_one(uint32_t x, uint32_t k) {
    return x | (1 << k);
}

uint32_t negate_kth_bit(uint32_t x, uint32_t k){
    return x ^ (1 << k);
}

int main() {

    assert(set_kth_to_zero(5,2) == 1);
    assert(set_kth_to_zero(4,2) == 0);
    assert(set_kth_to_zero(8,2) == 8);

    assert(set_kth_to_one(0,1) == 2);
    assert(set_kth_to_one(4,1) == 6);

    assert(negate_kth_bit(4,2) == 0);
    assert(negate_kth_bit(4,0) == 5);

    return 0;
}
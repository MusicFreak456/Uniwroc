#include<stdint.h>

uint32_t test(uint32_t x, uint32_t y){
    if(x < y) {
        return -1;
    } else if(x > y) {
        return 1;
    }
    return 0;
}

int main() {
    
    test(10,10);

    return 0;
}
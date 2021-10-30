#include<stdint.h>
#include<assert.h>

/*
Najpierw w zmiennej x przechowujemy informację, na którym bicie 
x i y się różnią. Później zmieniamy te bity, na których się 
różniły.
*/

int main() {

    uint32_t x = 10;
    uint32_t y = 42;

    x = x ^ y;
    y = x ^ y;
    x = x ^ y;

    assert(x == 42);
    assert(y == 10);

    return 0;
}
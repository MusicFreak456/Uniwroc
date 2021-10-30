#include<stdint.h>
#include<assert.h>
#include<stdbool.h>
#include<limits.h>
#include<stdio.h>

const int N = 32;

int sign(int32_t x){
    return -(x >> N-1 & 1) | -x >> N-1 & 1; // andowanie z 1 bo right shift uzupełnia jedynkami ujemne
}

/*
* Jeśli liczba jest ujemna to chcemy zwrócić -1, czyli -MSB. Prawa strona alternatywy 
bitowej wtedy jest równa 1 (dla INTN_MIN), lub 0, ale nie wpływa to na wynik, gdzyż -1 
to same jedynki w kodzie uzupełnień do 2.
* Jeśli liczba jest dodatnia to lewa strona alternatywy to 0, bo MSB x to 0. Liczba 
przeciwna do x musi mieć MSB = 1 (działa, bo -INTN_MAX, mieści się w intN_t). Czyli 
prawą stronę możemy ustawić na MSB(-x).
* Jeśli jest zerem to obie strony są równe zero, więc alternatywa bitowa zwróci 0.
*/

int main() {

    assert(sign(0) == 0);
    assert(sign(30) == 1);
    assert(sign(-20) == -1);
    assert(sign(INT32_MAX) == 1);
    assert(sign(INT32_MIN) == -1);

    return 0;
}
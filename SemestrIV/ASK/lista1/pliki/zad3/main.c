#include<stdint.h>
#include<assert.h>
#include<stdio.h>

/*
Jak dane układane są w pamięci?
* adresy zmiennych o typie prymitywnym muszą być 
  wielokrotnością ich rozmiaru.
* struktura musi kończyć się na adresie będącym
  wielokrotnością rozmiaru jej największej skł-
  -adowej.
*/

struct A
{
    int8_t a; // 1B, pozycja p
    // [3B padding], bo void* musi być na wielokrotności 4
    void *b; // 4B, pozycja p+4
    int8_t c; // 1B, pozycja p+8
    // [1B padding], bo int16, busi być na wielokrotności 2
    int16_t d; // 2B, pozycja p+10
    // Łącznie 12B, brak paddingu na końcu, bo to wielokrotność 4
};

/* Jak układać wydajniej? Od największej do najmniejszej. */

struct Better_A
{
    void *b; // 4B, pozycja p
    int16_t d; // 2B, pozycja p+4
    int8_t a; // 1B, pozycja p+6
    int8_t c; // 1B, poztja p+7
    // Łącznie 8B, brak paddingu na końcu, bo to wielokrotność 4
};

struct B {
    uint16_t a; // 2B, pozycja p
    // [6B padding], bo double musi być na wielokrotności 8
    double b; // 8B, pozycja p+8
    void *c; //4B, pozycja p+16
    // [4B padding], bo struktura musi się kończyć na wielokrotności 8
    // Łączneie 24B
};

struct Better_B {
    double b; // 8B, pozycja p
    void *c; //4B, pozycja p+8
    uint16_t a; // 2B, pozycja p+12
    // [4B padding], bo struktura musi się kończyć na wielokrotności 8
    // Łączneie 16B
};


int main() {

    struct A a;
    struct Better_A better_a;
    struct B b;
    struct Better_B better_b;

    assert(sizeof a == 12);
    assert(sizeof better_a == 8);
    assert(sizeof b == 24);
    assert(sizeof better_b == 16);

    return 0;
}
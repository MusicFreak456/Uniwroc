#include<stdlib.h>
#include<stdio.h>
#include<math.h>

int main() {
    int a = 42;
    int b = 33;
    int result = 0;
    int two_pow = 1;

    while(a > 0) {
        if(a%2 != 0) {
            result += two_pow * b;
        }
        two_pow *= 2;
        a /= 2;
    }

    printf("%d", result);

    return 0;
}
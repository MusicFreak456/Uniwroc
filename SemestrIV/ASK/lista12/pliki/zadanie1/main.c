#include<stdio.h>

void swap(long *xp, long *yp) {
    *xp = *xp + *yp; /* x+y */
    *yp = *xp - *yp; /* x+y-y = x */
    *xp = *xp - *yp; /* x+y-x = y */
}

void swap2(long *xp, long *yp) {
    long x = *xp, y = *yp;
    x = x + y, y = x - y, x = x - y;
    *xp = x, *yp = y;
}

int main(){
    long a = 2;
    long b = 2;

    swap(&a, &a);
    printf("%ld\n", a);

    swap2(&b, &b);
    printf("%ld\n", b);

    return 0;
}
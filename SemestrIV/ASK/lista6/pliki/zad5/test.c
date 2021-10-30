#include <alloca.h>

long aframe(long n, long idx, long *q) {
    long **p = alloca(n * sizeof(long *));
    long i;
    p[n-1] = &i;
    for (i = 0; i < n; i++)
        p[i] = q;
    return *p[idx];
}

int main(){
    return 0;
}
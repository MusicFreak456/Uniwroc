#include<stdio.h>

long puzzle(long n, long *p);

int main(){
    long tmp;
    puzzle(2, &tmp);
    printf("%ld\n", tmp);
}
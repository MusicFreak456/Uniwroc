#include<stdio.h>

void readlong(long *x){
    scanf("%ld",x);
}

long puzzle5(){
    long x,y;
    
    readlong(&x);
    readlong(&y);
    
    return x % y == 0;
}

int main(){
    printf("%ld\n",puzzle5());

    return 0;
}
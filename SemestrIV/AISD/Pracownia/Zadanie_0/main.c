#include<stdio.h>

int main()
{
    int a,b;

    scanf("%d %d", &a, &b);
    
    if(a > b) {
        b = b - a;
        a = a + b;
        b = a - b;
    }

    for(; a <= b; a++){
        printf("%d\n", a);
    }

    return 0;
}
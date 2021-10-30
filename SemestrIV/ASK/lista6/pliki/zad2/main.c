#include<stdio.h>
#include<limits.h>

struct T {
    long min;
    long max;
    long average;
};


struct T puzzle8(long *a, long n){
    long sum = 0;
    long max = LONG_MIN;
    long min = LONG_MAX;
    
    for(int i = 0; i < n; i++){
        long elem = a[i];
        if(elem > max) max = elem;
        if(elem < min) min = elem;
        sum += elem;
    }
    
    return (struct T) {.min = min, .max = max, .average = sum / n};
}

int main(){
    long Tab[3] = {1,2,3};
    struct T tmp;
    
    tmp = puzzle8(Tab, 3);
    printf("%ld\n", tmp.max);
}
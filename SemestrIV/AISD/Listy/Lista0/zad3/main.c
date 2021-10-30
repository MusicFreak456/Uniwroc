#include<stdlib.h>
#include<stdio.h>

int main() {
    int T[10] = {12, 5, 6, 4, 4, 2, 1, 5, 10, 8};
    int n = 9;

    for(int i=0; i<=n-1; i++) {
        for(int j=0; j <= n-1-i; j++){
            if(T[j] > T[j+1]) {
                int temp = T[j];
                T[j] = T[j+1];
                T[j+1] = temp;
            }
        }
    }

    for(int i=0; i<=9; i++){
        printf("%d\n",
            T[i]
        );
    }
    return 0;
}
#include<stdio.h>

int puzzle(long x, unsigned n){
	if(n==0) return 0;
	int bit_count = 0;
	for(int i=0; i<n; i++){
		bit_count += x & 1;
		x >>= 1;
	}
	return bit_count;
}

int main(){
    printf("%d\n", puzzle(-1,32));
    return 0;
}
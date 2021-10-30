#include<stdio.h>
#include<limits.h>


void dummy_2(){
    printf("hello\n");
}

void dummy_1(){
    dummy_2();
}

int main(){
    dummy_1();
}
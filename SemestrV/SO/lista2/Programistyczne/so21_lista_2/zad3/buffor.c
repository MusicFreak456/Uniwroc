#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<sys/wait.h>

int main(){

    printf("Hello\n");

    int pid = fork();
    if(pid == 0){
        printf("This is child\n");
    }
    else{
        waitpid(pid, NULL, 0);
        printf("This is parent\n");
    }

    return 0;
}
#include<pthread.h>
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>

void *sleep_one_sec(){
    for(int i=0; i < 1000000; i++){
        printf("%d\n", i);
    }
    return NULL;
}

int main() {
    pthread_t thread1_id;
    pthread_t thread2_id;

    pthread_create(&thread1_id, NULL, sleep_one_sec, NULL);
    pthread_create(&thread2_id, NULL, sleep_one_sec, NULL);

    pthread_join(thread1_id, NULL);
    pthread_join(thread2_id, NULL);
    
    return 0;
}
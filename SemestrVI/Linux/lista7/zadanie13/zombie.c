#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>

int main() {

  pid_t pid = fork();

  if(!pid) {
    printf("%d: I've seen enough. It's time to die.\n", getpid());
    return EXIT_SUCCESS;
  }

  char buffer[2];
  fgets(buffer, 2, stdin);

  return EXIT_SUCCESS;
}
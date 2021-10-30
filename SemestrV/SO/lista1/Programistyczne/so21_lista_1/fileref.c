#include "csapp.h"

static char buf[256];

#define LINE1 49
#define LINE2 33
#define LINE3 78

static void do_read(int fd) {
  /* TODO: Spawn a child. Read from the file descriptor in both parent and
   * child. Check how file cursor value has changed in both processes. */

  printf("Parent (pid: %d) - position before read: %ld\n", getpid(), lseek(fd, 0, SEEK_CUR));

  int pid = Fork();

  if(pid == 0){ //child
    Read(fd, buf, LINE1);
    printf("Child (pid: %d) - position after read: %ld\n", getpid(), lseek(fd, 0, SEEK_CUR));
  } 
  else { //parent
    int child_status;
    waitpid(pid, &child_status, 0);
    Read(fd, buf, LINE2);
    printf("Parent (pid: %d) - position after read: %ld\n", getpid(), lseek(fd, 0, SEEK_CUR));
  }

  Close(fd);
  exit(0);
}

static void do_close_1(int fd){

  int pid = Fork();

  if(pid == 0){ //child
    sleep(5);
    Read(fd, buf, LINE1);
    printf("Child (pid: %d) - line read: %s", getpid(), buf);
    Close(fd);
  }
  else {
    Close(fd);
    printf("Parent (pid: %d) - File closed\n", getpid());
  }

  exit(0);
}

static void do_close_2(int fd) {
  /* TODO: In the child close file descriptor, in the parent wait for child to
   * die and check if the file descriptor is still accessible. */

  int pid = Fork();

  if(pid == 0){ //child
    Close(fd);
    printf("Child (pid: %d) - File closed\n", getpid());
  }
  else { //parent
    int child_status;
    waitpid(pid, &child_status, 0);
    Read(fd, buf, LINE1);
    printf("Parent (pid: %d) - line read: %s", getpid(), buf);
    Close(fd);
  }

  exit(0);
}

int main(int argc, char **argv) {
  if (argc != 2)
    app_error("Usage: %s [read|close1|close2]", argv[0]);

  int fd = Open("test.txt", O_RDONLY, 0);

  if (!strcmp(argv[1], "read"))
    do_read(fd);
  if(!strcmp(argv[1], "close1"))
    do_close_1(fd);
  if (!strcmp(argv[1], "close2"))
    do_close_2(fd);
  app_error("Unknown variant '%s'", argv[1]);
}

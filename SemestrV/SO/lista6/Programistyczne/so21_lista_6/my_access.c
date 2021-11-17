#include "csapp.h"

bool my_access(struct stat *sb, int mode) {
  uid_t euid = geteuid();
  mode_t file_mode = sb->st_mode;
  mode_t file_permission_bits = file_mode & 0777;

  if(!euid) return true;

  gid_t file_uid = sb->st_uid;
  if(file_uid == euid) 
    return ((file_permission_bits >> 6) & mode) == mode;

  int number_of_groups = getgroups(0, NULL);
  gid_t groups[number_of_groups];
  if(getgroups(number_of_groups, groups) != number_of_groups)
    unix_error("getgroups error");

  gid_t file_gid = sb->st_gid;
  for (int i = 0; i < number_of_groups; i++)
  {
    if(file_gid == groups[i]) 
      return ((file_permission_bits >> 3) & mode) == mode;
  }

  return (file_permission_bits & mode) == mode;
}

int main(int argc, char const *argv[]) {
    if(argc < 1 || argc > 2) return 1;

    struct stat sb;
    stat(argv[1], &sb);

    printf("%d\n", my_access(&sb, 06));

    return 0;
}

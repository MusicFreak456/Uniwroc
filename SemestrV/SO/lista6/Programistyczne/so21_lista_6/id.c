#include "csapp.h"

static const char *uidname(uid_t uid) {
  /* TODO: Something is missing here! */
  return getpwuid(uid)->pw_name;
}

static const char *gidname(gid_t gid) {
  /* TODO: Something is missing here! */
  return getgrgid(gid)->gr_name;
}

static int getid(uid_t *uid_p, gid_t *gid_p, gid_t **gids_p) {
  gid_t *gids = NULL;
  int groups = 0;

  /* TODO: Something is missing here! */
  *uid_p = getuid();
  *gid_p = getgid();

  /* 'If  size  is  zero, list is not modified, but the total number of 
   *  supplementary group IDs for the process is returned.' 
   * ~ man 2 setgroups
   */
  groups = getgroups(0, gids);
  gids = malloc(groups * sizeof(gid_t));
  groups = getgroups(groups, gids);

  *gids_p = gids;
  return groups;
}

int main(void) {
  uid_t uid;
  gid_t *gids, gid;
  int groups = getid(&uid, &gid, &gids);

  printf("uid=%d(%s) gid=%d(%s) ", uid, uidname(uid), gid, gidname(gid));
  printf("groups=%d(%s)", gids[0], gidname(gids[0]));
  for (int i = 1; i < groups; i++)
    printf(",%d(%s)", gids[i], gidname(gids[i]));
  putchar('\n');
  (void)groups;

  free(gids);

  return 0;
}

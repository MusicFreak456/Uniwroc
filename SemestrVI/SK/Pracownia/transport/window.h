#ifndef WINDOW_H
#define WINDOW_H

#include<stdbool.h>
#include<pthread.h>
#include"common.h"

#define WINDOW_SIZE 1000
#define WATCHDOG_INTERVAL 100000000

#define MAX_AGE 8

typedef struct {
  void *data;
  int start;
  int size;
  int age;
  bool received;
} part_s;

typedef struct {
  sockaddr_in server_address;
  part_s parts[WINDOW_SIZE];
  pthread_mutex_t lock;
  int sockfd;
  int number_of_parts;
  int start_idx;
  int end_idx;
  int queued_bytes;
  int output_fd;
} window_s;

window_s window_init(int sockfd);
void window_foreach(window_s *window, void (*callback)(window_s*, part_s*));
void window_download(
  window_s *window, sockaddr_in server_address, int number_of_bytes, 
  int output_fd
);
void send_part_request(window_s *window, part_s *part);
void init_watchdog(window_s *window);

#endif
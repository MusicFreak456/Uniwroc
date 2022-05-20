/* Cezary Świtała 316746 */
#include"window.h"
#include<stdbool.h>
#include<unistd.h>
#include<string.h>
#include<netinet/ip.h>
#include<math.h>

window_s window_init(int sockfd) {
  window_s new_window;
  pthread_mutex_init(&new_window.lock, NULL);
  new_window.sockfd = sockfd;
  new_window.number_of_parts = 0;
  new_window.start_idx = 0;
  new_window.end_idx = 0;
  new_window.queued_bytes = 0;
  return new_window;
}

static inline int next_idx(int current_idx) {
  return (current_idx == WINDOW_SIZE - 1) ? 0 : (current_idx + 1);
}

static inline int prev_idx(int current_idx) {
  return (current_idx == 0) ? WINDOW_SIZE - 1 : (current_idx - 1);
}

static int calc_idx(window_s *window, int start) {
  if(window->number_of_parts == 0) return -1;

  int min_start = window->parts[window->start_idx].start;
  int diff = start - min_start;

  if(diff < 0) return -1;

  int shift = diff / PACKET_SIZE;
  if(shift >= window->number_of_parts) return -1;

  return (shift + window->start_idx) % WINDOW_SIZE;
}

bool window_append(window_s *window, int start, int size) {
  if(window->number_of_parts >= WINDOW_SIZE){
    return false;
  }

  part_s *new_part = &window->parts[window->end_idx];
  new_part->data = NULL;
  new_part->start = start;
  new_part->size = size;
  new_part->age = 0;
  new_part->received = false;

  window->end_idx = next_idx(window->end_idx);
  window->number_of_parts++;

  return true;
}

void send_part_request(window_s *window, part_s *part) {
  char message_buff[REQ_RES_STR_MAXLEN];
  sprintf(message_buff, "GET %d %d\n", part->start, part->size);
  ssize_t message_size = strlen(message_buff);

  debug("%s", message_buff);

  int bytes_sent = 0;
  int sockfd = window->sockfd;

  while (bytes_sent < message_size) {
    bytes_sent = 
      sendto(sockfd, message_buff, message_size, 0,
             (struct sockaddr*)&window->server_address, sizeof(sockaddr_in));

    if(bytes_sent < 0 && errno != EINTR) {
      error("sendto", strerror(errno));
    }
  }
}

void window_foreach(window_s *window, void (*callback)(window_s*, part_s*)) {
  int cursor = window->start_idx;

  for(int i = 0; i < window->number_of_parts; i++){
    part_s *part = &window->parts[cursor];
    callback(window, part);
    cursor = next_idx(cursor);
  }
}

bool accept_package(sockaddr_in expected_addr, sockaddr_in received_addr, 
                    int len) {
  if(len != sizeof(sockaddr_in)) return false;
  if(expected_addr.sin_family != received_addr.sin_family 
  || expected_addr.sin_port != received_addr.sin_port
  || expected_addr.sin_addr.s_addr != received_addr.sin_addr.s_addr
  ) return false;

  return true;
}

void decode_package(void *buffer, int *start, int *size, void **data) {
  char *header = strtok((char *)buffer, "\n");
  if(header == NULL) {
    error("strtok", strerror(errno));
  }

  debug("%s\n", header);
  
  int scan_num = sscanf(header, "DATA %d %d", start, size);
  if(scan_num != 2) {
    error("sscanf", "Header format mismatch");
  }

  *data = buffer + strlen(header) + 1;
}

void receive_part(window_s *window, int idx, void *data) {
  if(idx == -1) return;

  part_s *part = &window->parts[idx];

  if(!part->received) {
    part->received = true;
    part->data = malloc(part->size);
    memcpy(part->data, data, part->size);
  }
}

void print_progress(part_s *part, window_s *window) {
  part_s *last_part = &window->parts[prev_idx(window->end_idx)];
  int total = last_part->start + last_part->size + window->queued_bytes;
  int bytes_received = part->start + part->size;
  printf("\rDownloading... %.0f%%", (bytes_received/(float)total) * 100);
  fflush(stdout);
  if(total == bytes_received) printf("\n");
}

void save(int fd, void* data, int size){
  int bytes_written = 0;
  int bytes_to_be_written = size;

  while(bytes_to_be_written > 0) {
    bytes_written = write(fd, data + bytes_to_be_written, bytes_to_be_written);

    if(bytes_written < 0 && errno != EINTR) {
      error("write: ", strerror(errno));
    } else {
      bytes_to_be_written -= bytes_written;
    }
  }
}

void move_window(window_s *window) {
  while(
    window->number_of_parts > 0 && 
    window->parts[window->start_idx].received
  ) {
    part_s *first_part = &window->parts[window->start_idx];

    print_progress(first_part, window);
    write(window->output_fd, first_part->data, first_part->size);
    free(first_part->data);

    window->number_of_parts--;
    window->start_idx = next_idx(window->start_idx);

    if(window->queued_bytes > 0) {
      int new_part_size = min(PACKET_SIZE, window->queued_bytes);
      window->queued_bytes -= new_part_size;
      part_s *last_part = &window->parts[prev_idx(window->end_idx)];
      int new_part_start = last_part->start + last_part->size;
      window_append(window, new_part_start, new_part_size);
      send_part_request(window, &window->parts[prev_idx(window->end_idx)]);
    }
  }
}

bool is_collecting_done(window_s *window) {
  return window->number_of_parts == 0 && window->queued_bytes == 0;
}

void collect_packages(window_s *window) {
  sockaddr_in sender;
  uint8_t buffer[IP_MAXPACKET + 1];

  while (!is_collecting_done(window)) {
    socklen_t sender_len = sizeof(sender);
    ssize_t datagram_len = 
      recvfrom(window->sockfd, buffer, IP_MAXPACKET, 0, 
              (struct sockaddr*)&sender, &sender_len);

    if(datagram_len < 0) {
      error("recvfrom", strerror(errno));
    }

    pthread_mutex_lock(&window->lock);

    if(accept_package(window->server_address, sender, sender_len)) {
      int start, size;
      void *data;
      decode_package(buffer, &start, &size, &data);
      int part_idx = calc_idx(window, start);
      receive_part(window, part_idx, data);
      move_window(window);
    }

    pthread_mutex_unlock(&window->lock);
  }
}

void window_download(
    window_s *window, sockaddr_in server_address, int number_of_bytes, 
    int output_fd
  ) {
  debug("Downloading %d bytes\n", number_of_bytes);
  window->server_address = server_address;
  window->output_fd = output_fd;

  int start = 0;

  while (start < number_of_bytes) {
    int next_start = start + PACKET_SIZE;
    int size = (next_start > number_of_bytes)
               ? number_of_bytes % PACKET_SIZE
               : PACKET_SIZE;
    
    if(!window_append(window, start, size)){
      window->queued_bytes = number_of_bytes - start;
      debug("Bytes queued %d\n", window->queued_bytes);
      break;
    }

    debug("Add part: start %d size %d\n", start, size);

    start += PACKET_SIZE;
  }

  window_foreach(window, send_part_request);
  init_watchdog(window);
  collect_packages(window);
}

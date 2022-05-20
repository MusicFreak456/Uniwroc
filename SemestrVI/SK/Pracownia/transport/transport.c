/* Cezary Świtała 316746 */
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<unistd.h>
#include<fcntl.h>
#include"common.h"
#include"window.h"

sockaddr_in parse_address(string address_str, string port_str) {
  sockaddr_in address;

  memset(&address, 0, sizeof(sockaddr_in));
  address.sin_family = AF_INET;
  address.sin_port = htons(atoi(port_str));
  int retval = inet_pton(AF_INET, address_str, &address.sin_addr);

  if(retval < 0) {
    error("inet_pton", strerror(errno));
    exit(EXIT_FAILURE);
  } else if (retval == 0) {
    error("inet_pton", "Host address must be a valid IPv4 address.");
    exit(EXIT_FAILURE);
  }

  return address;
}

int socket_init() {
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);

  if(sockfd < 0) {
    error("socket", strerror(errno))
  }

  return sockfd;
}

int main(int argc, char *argv[]) {
  if(argc != 5) {
    printf("Usage: %s ip port file number-of-bytes\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  sockaddr_in server_address = parse_address(argv[1], argv[2]);

  int output_file_fd = open(argv[3], O_RDWR | O_TRUNC | O_CREAT, 0666);
  if(output_file_fd < 0) {
    error("open", strerror(errno));
  }

  int sockfd = socket_init();

  window_s window = window_init(sockfd);
  window_download(&window, server_address, atoi(argv[4]), output_file_fd);

  return EXIT_SUCCESS;
}

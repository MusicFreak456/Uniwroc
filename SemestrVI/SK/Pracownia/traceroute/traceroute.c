#include "traceroute.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

float calculate_mean(response_t *responses, int res_num) {
  float mean = 0;

  for (int i = 0; i < res_num; i++){
    mean += responses[i].response_time;
  }

  return mean / res_num; 
}

int init_socket() {
  int sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);

  if(sockfd < 0) {
    error("Socket", strerror(errno));
    exit(EXIT_FAILURE);
  }

  address_t bind_address;
  memset(&bind_address, 0, sizeof(address_t));
  bind_address.sin_family = AF_INET;
  bind_address.sin_addr.s_addr = INADDR_ANY;
  if(bind(sockfd, (struct sockaddr*)&bind_address, sizeof(bind_address)) < 0) {
    error("Bind", strerror(errno));
  }

  return sockfd;
}

bool destination_reached(char *host, response_t *responses, int res_num) {
  for (int i = 0; i < res_num; i++) {
    if(strcmp(responses[i].ip_address_str, host) == 0)
      return true;
  }
  return false;
}

void print_responses(response_t *responses, int res_num, int ttl) {
  printf("%d. ", ttl);

  if(res_num == 0){
    printf("*\n");
    return;
  } else if (res_num != BURST_LEN) {
    printf("???\n");
    return;
  }

  for (int i = 0; i < res_num; i++) {
    bool print = true;
    for (int j = 0; j < i; j++) {
      if(!strcmp(responses[i].ip_address_str, responses[j].ip_address_str)) {
        print = false;
        break;
      }
    }
    if (print)
      printf("%s ", responses[i].ip_address_str);
  }

  printf("%.2f ms\n", calculate_mean(responses, res_num));
}

void send_packets(int sockfd, address_t address, int ttl) {
  int seq = (ttl - 1) * 3;
  setsockopt(sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));

  for(int i=0; i<BURST_LEN; i++) {
    send_packet(sockfd, address, seq);
    seq++;
  }
}

int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("Usage: %s host\n", argv[0]);
  }

  address_t host = parse_address(argv[1]);
  int sockfd = init_socket();
  int res_num;

  for(int ttl=1; ttl<=30; ttl++) {
    send_packets(sockfd, host, ttl);
    response_t responses[BURST_LEN];
    res_num = collect_responses(sockfd, responses, ttl);
    print_responses(responses, res_num, ttl);
    if(destination_reached(argv[1], responses, res_num))
      break;
  }

  return EXIT_SUCCESS;
}

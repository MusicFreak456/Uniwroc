#ifndef TRACEROUTE_H
#define TRACEROUTE_H

#include<netinet/ip.h>
#include<errno.h>
#include<stdio.h>

#define BURST_LEN 3
#define IP_ADDR_STRLEN 16

#define error(proc, msg) fprintf(stderr, "%s : %s\n", proc, msg);

typedef struct sockaddr_in address_t;
typedef struct {
  float response_time;
  char ip_address_str[IP_ADDR_STRLEN];
} response_t;


address_t parse_address(char *address);
uint16_t compute_icmp_checksum (const void *buff, int length);
void send_packet(int sockfd, address_t host, int seq);
int collect_responses(int sockfd, response_t *responses, int ttl);

#endif
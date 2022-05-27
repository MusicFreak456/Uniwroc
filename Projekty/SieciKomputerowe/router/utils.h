/* Cezary Świtała 316746 */
#ifndef UTILS_H
#define UTILS_H

#include<stdint.h>
#include<errno.h>
#include<stdio.h>

#define error(proc, msg) \
  fprintf(stderr, "%s : %s\n", proc, msg); \
  exit(EXIT_FAILURE);

#define INFTY 16U

#define IP_STRING_LEN 20

typedef struct sockaddr_in address_t;

typedef struct __attribute__((__packed__)) {
  uint32_t ip;
  uint8_t subnet_mask;
  uint32_t distance;
} net_s;

uint32_t to_binary_mask(uint8_t mask);
uint32_t to_broadcast_address(uint32_t address, uint8_t mask);
uint32_t to_network_address(uint32_t address, uint8_t mask);
net_s netify(net_s net);
net_s unnetify(net_s net);

#endif
/* Cezary Świtała 316746 */
#ifndef NEIGHBOURS_H
#define NEIGHBOURS_H

#include<netinet/ip.h>
#include<stdint.h>
#include"utils.h"

typedef struct {
  net_s net;
  address_t broadcast_address;
} neighbour_s;

typedef struct {
  neighbour_s *neighbours;
  int number_of_neighbours;
} neighbours_s;

void read_neighbours();
void foreach_neigbour(void (*callback)(neighbour_s*));
void print_neigbour(neighbour_s *neighbour);
void notify_neigbours(int sockfd);
neighbour_s *find_neighbour(address_t sender);

#endif
/* Cezary Świtała 316746 */
#ifndef DIST_VEC_H
#define DIST_VEC_H

#include<stdint.h>
#include<stdbool.h>
#include<sys/queue.h>
#include"utils.h"

#define VEC_START_SIZE 5

#define TOLARANCE 2
#define TIMEOUT 5

typedef struct {
  net_s net;
  uint32_t next_hop;
  bool is_direct;
} vector_entry_s;

void vector_init();
void vector_insert(vector_entry_s entry);
void vector_foreach_entry(void (*callback)(vector_entry_s*));
void vector_print();
bool vector_send(int sockfd, address_t dest);
void vector_update(net_s *net, uint32_t next_hop, uint32_t distance);
void vector_scan();
void vector_touch_interface(net_s *neigbour_net);

#endif

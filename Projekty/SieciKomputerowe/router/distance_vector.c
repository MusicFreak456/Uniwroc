/* Cezary Świtała 316746 */
#include"distance_vector.h"
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<pthread.h>
#include<arpa/inet.h>

struct list_entry {
  vector_entry_s vec_entry;
  TAILQ_ENTRY(list_entry) links;
  int last_access;
};

TAILQ_HEAD(, list_entry) vector_head;
int curr_round;
pthread_mutex_t lock;

void vector_init() {
  TAILQ_INIT(&vector_head);
  curr_round = 0;
  if(pthread_mutex_init(&lock, NULL) != 0) {
    error("mutex_init", strerror(errno));
  }
}

void vector_insert(vector_entry_s entry) {
  struct list_entry *new_entry = malloc(sizeof(struct list_entry));
  new_entry->vec_entry = entry;
  new_entry->last_access = curr_round;
  pthread_mutex_lock(&lock);
  TAILQ_INSERT_HEAD(&vector_head, new_entry, links);
  pthread_mutex_unlock(&lock);
}

void vector_foreach_entry(void (*callback)(vector_entry_s*)) {
  struct list_entry *entry;
  pthread_mutex_lock(&lock);
  TAILQ_FOREACH(entry, &vector_head, links) {
    callback(&entry->vec_entry);
  }
  pthread_mutex_unlock(&lock);
}

void print_vector_entry(vector_entry_s *entry) {
  char net_address[IP_STRING_LEN];
  inet_ntop(AF_INET, &entry->net.ip, net_address, IP_STRING_LEN);

  printf("%s/%d", net_address, entry->net.subnet_mask);

  if(entry->net.distance < INFTY) {
    printf(" distance %d", entry->net.distance);
  } else {
    printf(" unreachable");
  }
  
  if(!entry->is_direct) {
    char hop_address[IP_STRING_LEN];
    inet_ntop(AF_INET, &entry->next_hop, hop_address, IP_STRING_LEN);
    printf(" via %s\n", hop_address);
    return;
  }

  printf(" connected directly\n");
}

void vector_print() {
  vector_foreach_entry(print_vector_entry);
  printf("---------------------------------------------\n");
}

bool vector_send(int sockfd, address_t dest) {
  struct list_entry *entry;
  pthread_mutex_lock(&lock);
  TAILQ_FOREACH(entry, &vector_head, links) {
    unsigned int bytes_acc = 0;

    net_s description = entry->vec_entry.net;
    description = netify(description);

    while(bytes_acc < sizeof(net_s)) {
      int msg_size = sizeof(net_s);
      void *message = (void*)&description;
      
      int bytes_sent = sendto(sockfd, message, msg_size, 0, 
                             (struct sockaddr*)&dest, sizeof(dest));

      if(bytes_sent < 0 && errno != EINTR) {
        pthread_mutex_unlock(&lock);
        return false;
      }

      bytes_acc += bytes_sent;
    }
  }
  pthread_mutex_unlock(&lock);
  return true;
}

static inline bool match_nets(net_s *net1, net_s *net2) {
  return net1->ip == net2->ip && net1->subnet_mask == net2->subnet_mask;
}

void update_entry(struct list_entry *entry, uint32_t next_hop, 
                  uint32_t new_dist) {
  net_s *updated_net = &entry->vec_entry.net;

  if(!entry->vec_entry.is_direct && 
      entry->vec_entry.next_hop == next_hop) {
    updated_net->distance = new_dist;
    if(new_dist < INFTY) entry->last_access = curr_round; 
  } else if (new_dist < updated_net->distance) {
    updated_net->distance = new_dist;
    entry->vec_entry.next_hop = next_hop;
    entry->vec_entry.is_direct = false;
    entry->last_access = curr_round; 
  }
}

void vector_update(net_s *net, uint32_t next_hop, uint32_t distance) {
  struct list_entry *entry;

  pthread_mutex_lock(&lock);
  TAILQ_FOREACH(entry, &vector_head, links) {
    net_s *current = &entry->vec_entry.net;

    if(match_nets(current, net)) {

      uint32_t new_distance = net->distance + distance;
      new_distance = new_distance > INFTY ? INFTY : new_distance;
      update_entry(entry, next_hop, new_distance);

      pthread_mutex_unlock(&lock);
      return;
    }
  }
  pthread_mutex_unlock(&lock);

  if(net->distance >= INFTY) return;

  vector_entry_s new_entry;
  new_entry.is_direct = false;
  new_entry.net = *net;
  new_entry.net.distance += distance;
  new_entry.next_hop = next_hop;
  vector_insert(new_entry);
}

void vector_scan() {
  struct list_entry *entry;
  struct list_entry *next_entry;

  pthread_mutex_lock(&lock);
  for(entry = TAILQ_FIRST(&vector_head); entry != NULL; entry = next_entry) {
    next_entry = TAILQ_NEXT(entry, links);

    if(entry->last_access + TIMEOUT < curr_round){
      TAILQ_REMOVE(&vector_head, entry, links);
      free(entry);
      continue;
    }

    if(entry->last_access + TOLARANCE < curr_round){
      entry->vec_entry.net.distance = INFTY;
      continue;
    }
  }
  curr_round++;
  pthread_mutex_unlock(&lock);
}

void vector_touch_interface(net_s *net) {
  struct list_entry *entry;
  pthread_mutex_lock(&lock);
 
  TAILQ_FOREACH(entry, &vector_head, links) {
    if(match_nets(net, &entry->vec_entry.net)){
      entry->last_access = curr_round;
      entry->vec_entry.net.distance = net->distance;
      entry->vec_entry.is_direct = true;
      pthread_mutex_unlock(&lock);
      return;
    }
  }

  pthread_mutex_unlock(&lock);

  vector_entry_s new_entry;
  new_entry.net = *net;
  new_entry.is_direct = true;
  new_entry.next_hop = 0;
  vector_insert(new_entry);
}

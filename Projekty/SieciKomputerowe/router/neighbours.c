/* Cezary Świtała 316746 */
#include"neighbours.h"
#include"distance_vector.h"
#include<limits.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<arpa/inet.h>

static neighbours_s neighbours;

void new_neighbour(int idx, uint32_t address, uint8_t mask, int distance) {
  neighbour_s *neighbour = &neighbours.neighbours[idx];
  neighbour->net.ip = to_network_address(address, mask);
  neighbour->net.subnet_mask = mask;
  neighbour->net.distance = distance;

  memset(&neighbour->broadcast_address, 0, sizeof(address_t));
  neighbour->broadcast_address.sin_family = AF_INET;
  neighbour->broadcast_address.sin_port = htons(54321);
  neighbour->broadcast_address.sin_addr.s_addr = 
    to_broadcast_address(address, mask);
}

void read_neighbours() {
  int number_of_neigbours;
  scanf("%d\n", &number_of_neigbours);

  neighbours.neighbours = calloc(number_of_neigbours, sizeof(neighbour_s));
  neighbours.number_of_neighbours = number_of_neigbours;

  for(int i=0; i < number_of_neigbours; i++) {
    char line[LINE_MAX];
    uint32_t address;
    uint8_t mask;

    fgets(line, LINE_MAX, stdin);
    
    inet_pton(AF_INET, strtok(line, "/"), &address);
    mask = atoi(strtok(NULL, " "));
    strtok(NULL, " ");
    int distance = atoi(strtok(NULL, " "));

    new_neighbour(i, address, mask, distance);
  }
}

void foreach_neigbour(void (*callback)(neighbour_s*)) {
  for(int i = 0; i < neighbours.number_of_neighbours; i++) {
    callback(&neighbours.neighbours[i]);
  }
}

void print_neigbour(neighbour_s *neighbour) {
  char broadcast_address[IP_STRING_LEN];
  char network_address[IP_STRING_LEN];
  inet_ntop(AF_INET, &neighbour->broadcast_address.sin_addr, broadcast_address, 
            IP_STRING_LEN);
  inet_ntop(AF_INET, &neighbour->net.ip, network_address, IP_STRING_LEN);
  printf("%s %s/%d distance %d\n", broadcast_address, network_address, 
         neighbour->net.subnet_mask, neighbour->net.distance);
}

void notify_neigbours(int sockfd) {
  for(int i = 0; i < neighbours.number_of_neighbours; i++) {
    bool is_success = 
      vector_send(sockfd, neighbours.neighbours[i].broadcast_address);

    if(is_success) {
      vector_touch_interface(&neighbours.neighbours[i].net);
    }
  }
}

neighbour_s *find_neighbour(address_t sender){
  for(int i = 0; i < neighbours.number_of_neighbours; i++) {
    neighbour_s neighbour = neighbours.neighbours[i];
    uint32_t network_address = 
      to_network_address(sender.sin_addr.s_addr, neighbour.net.subnet_mask);
    
    if(network_address == neighbour.net.ip) {
      return &neighbours.neighbours[i];
    }
  }

  return NULL;
}

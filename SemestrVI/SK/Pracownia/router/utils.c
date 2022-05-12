/* Cezary Świtała 316746 */
#include"utils.h"
#include<arpa/inet.h>

uint32_t to_binary_mask(uint8_t mask) {
  return ~((1 << (32 - mask)) - 1);
}

uint32_t to_broadcast_address(uint32_t address, uint8_t mask) {
  return address | htonl(~to_binary_mask(mask));
}

uint32_t to_network_address(uint32_t address, uint8_t mask) {
  return address & htonl(to_binary_mask(mask));
}

net_s netify(net_s net) {
  net_s netified = net;
  if(netified.distance >= INFTY) {
    netified.distance = UINT32_MAX;
  }
  netified.distance = htonl(netified.distance);
  return netified;
}

net_s unnetify(net_s net) {
  net_s unnetified = net;
  unnetified.distance = ntohl(unnetified.distance);
  if(unnetified.distance >= INFTY){
    unnetified.distance = INFTY;
  }
  return unnetified;
}

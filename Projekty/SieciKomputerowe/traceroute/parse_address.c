/* Cezary Świtała 316746 */
#include<stdlib.h>
#include<string.h>
#include<arpa/inet.h>
#include"traceroute.h"

int try_parse_address(char *address, address_t *destination) {
  memset(destination, 0, sizeof(address_t));
  destination->sin_family = AF_INET;
  return inet_pton(AF_INET, address, &destination->sin_addr);
}

address_t parse_address(char *address) {
  address_t address_struct;
  int retval = try_parse_address(address, &address_struct);

  if(retval < 0) {
    error("inet_pton", strerror(errno));
    exit(EXIT_FAILURE);
  } else if (retval == 0) {
    error("inet_pton", "Host address must be a valid IPv4 address.");
    exit(EXIT_FAILURE);
  }

  return address_struct;
}

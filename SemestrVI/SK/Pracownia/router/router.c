/* Cezary Świtała 316746 */
#include"router.h"
#include<stdlib.h>
#include<unistd.h>
#include<arpa/inet.h>
#include<string.h>
#include"neighbours.h"
#include"distance_vector.h"
#include"utils.h"

void add_neigbour_to_dist_vec(neighbour_s *neighbour) {
  vector_touch_interface(&neighbour->net);
}

int socket_init() {
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);

  if(sockfd < 0) {
    error("socket", strerror(errno))
  }

  address_t server_address;
	memset(&server_address, 0, sizeof(address_t));
	server_address.sin_family = AF_INET;
	server_address.sin_port = htons(54321);
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);

  int retval = bind(sockfd, (struct sockaddr*)&server_address, 
                    sizeof(server_address));
	if(retval < 0) {
		error("bind", strerror(errno));
	}

  int broadcastPermission = 1;
  setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST, (void *)&broadcastPermission,
             sizeof(broadcastPermission));

  return sockfd;
}

int main(){
  read_neighbours();

  vector_init();
  foreach_neigbour(add_neigbour_to_dist_vec);
  
  int sockfd = socket_init();
  init_broadcaster(sockfd);

  while (true) {
    receive(sockfd);
  }

  return EXIT_SUCCESS;
}

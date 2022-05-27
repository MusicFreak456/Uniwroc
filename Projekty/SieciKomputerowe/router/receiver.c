/* Cezary Świtała 316746 */
#include"router.h"
#include"utils.h"
#include"neighbours.h"
#include"distance_vector.h"
#include<netinet/ip.h>
#include<string.h>

void receive(int sockfd) {
  address_t sender;
  socklen_t senderlen = sizeof(sender);
  uint8_t buffer[IP_MAXPACKET + 1]; 

  ssize_t datagram_size = recvfrom(sockfd, buffer, IP_MAXPACKET, 0, 
                                   (struct sockaddr*)&sender, &senderlen);

  if(datagram_size < 0) {
    error("recvfrom", strerror(errno));
  }

  neighbour_s *neighbour = find_neighbour(sender);
  if(!neighbour) {
    return;
  }

  net_s *description = (net_s *)buffer;
  *description = unnetify(*description);

  vector_update(description, sender.sin_addr.s_addr, neighbour->net.distance);
}

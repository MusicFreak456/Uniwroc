#include<stdbool.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<time.h>
#include<arpa/inet.h>
#include<netinet/ip_icmp.h>
#include"traceroute.h"

bool examine_packet(uint8_t *buffer, int ttl);

bool examine_icmp_header(struct icmp* header, int ttl) {
  uint8_t type = header->icmp_type;

  if(type == ICMP_TIMXCEED) {
    
    uint8_t *internal_packet = ((uint8_t *) header) + sizeof(header);
    return examine_packet(internal_packet, ttl);

  } else if (type == ICMP_ECHO || type == ICMP_ECHOREPLY) {
    pid_t pid = getpid();
    uint16_t id = ntohs(header->icmp_hun.ih_idseq.icd_id);
    uint16_t seq = ntohs(header->icmp_hun.ih_idseq.icd_seq);

    pid_t res_pid = id | ((seq & 0x0f) << 16);
    seq = (seq >> 4) / BURST_LEN + 1;

    if(res_pid == pid && seq == ttl) {
      return true;
    }
  }
  return false;
}

bool examine_packet(uint8_t *buffer, int ttl) {
  struct ip* ip_header = (struct ip*) buffer;
  ssize_t ip_header_len = 4 * ip_header->ip_hl;
  struct icmp* icmp_header = (struct icmp*)(buffer + ip_header_len);
  return examine_icmp_header(icmp_header, ttl);
}

int collect_responses(int sockfd, response_t *responses, int ttl) {
  fd_set descriptors;
  FD_ZERO(&descriptors);
  FD_SET(sockfd, &descriptors);
  struct timeval tv; tv.tv_sec=0; tv.tv_usec=1000000;
  int count = 0;
  int ready;
  
  while(count < BURST_LEN &&
       ((ready = select(sockfd+1, &descriptors, NULL, NULL, &tv)) > 0)) {
    
    uint8_t buffer[IP_MAXPACKET];
    address_t response_ip;
    socklen_t response_ip_len = sizeof(response_ip);
    ssize_t bytes_received;
    bytes_received = recvfrom(sockfd, buffer, IP_MAXPACKET, 0, 
                             (struct sockaddr*)&response_ip, &response_ip_len);

    if(bytes_received < 0) {
      error("Recvfrom", strerror(errno));
      break;
    }

    char response_ip_str[IP_ADDR_STRLEN];
    inet_ntop(AF_INET, &(response_ip.sin_addr), 
              response_ip_str, IP_ADDR_STRLEN);

    if(examine_packet(buffer, ttl)) {
      strcpy(responses[count].ip_address_str, response_ip_str);
      responses[count].response_time = (1000000.0 - tv.tv_usec) / 1000; 
      count+=1;
    }
  }

  if(ready < 0) {
    error("Select", strerror(errno));
    exit(EXIT_FAILURE);
  }

  return count;
}

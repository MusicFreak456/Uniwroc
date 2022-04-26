/* Cezary Świtała 316746 */
#include<netinet/ip_icmp.h>
#include<unistd.h>
#include<string.h>
#include<stdlib.h>
#include"traceroute.h"

struct icmp get_icmp_header(int seq) {
  struct icmp header;
  pid_t pid = getpid();
  header.icmp_type = ICMP_ECHO;
  header.icmp_code = 0;
  header.icmp_hun.ih_idseq.icd_id = htons((uint16_t) pid);
  header.icmp_hun.ih_idseq.icd_seq = htons((seq << 4) | (pid >> 16));
  header.icmp_cksum = 0;
  header.icmp_cksum = compute_icmp_checksum(&header, sizeof(header));
  return header;
}

void send_packet(int sockfd, address_t host, int seq) {
  unsigned int bytes_acc = 0;
  struct icmp header = get_icmp_header(seq);

  while(bytes_acc < sizeof(header)){
    int bytes_sent = sendto(sockfd, &header, sizeof(header), 0,
                           (struct sockaddr*)&host, sizeof(host));

    if(bytes_sent < 0 && errno != EINTR) {
      error("Sendto", strerror(errno));
      exit(EXIT_FAILURE);
    }

    bytes_acc+=bytes_sent;
  }
}

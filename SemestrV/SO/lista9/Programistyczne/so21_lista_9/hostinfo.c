#include "csapp.h"

int main(int argc, char **argv) {
  struct addrinfo *p, *listp, hints;
  char *node;
  char *service = NULL;
  char ip_address_buf[MAXLINE];
  char port_buf[MAXLINE];
  int rc, flags;

  if (argc < 2 || argc > 3)
    app_error("usage: %s <domain name> [service name]\n", argv[0]);
  node = argv[1];
  if (argc == 3) service = argv[2];

  /* Get a list of addrinfo records */
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC; /* IPv4 or IPv6 */
  hints.ai_socktype = SOCK_STREAM;
  /* Connections only */
  if ((rc = getaddrinfo(node, service, &hints, &listp)) != 0)
    gai_error(rc, "getaddrinfo");

  /* Walk the list and display each IP address */
  flags  = NI_NUMERICHOST; /* Display address string instead of domain name */
  flags |= NI_NUMERICSERV; /* Display port string instead of service name   */
  for (p = listp; p; p = p->ai_next) {
    Getnameinfo(
      p->ai_addr, p->ai_addrlen, 
      ip_address_buf, MAXLINE, port_buf, MAXLINE, 
      flags
    );
    
    printf(p->ai_family == AF_INET ? "%s" : "[%s]", ip_address_buf);
    if(service) printf(":%s", port_buf);
    printf("\n");
  }

  /* Clean up */
  freeaddrinfo(listp);

  return EXIT_SUCCESS;
}

/* Cezary Świtała 316746 */
#ifndef ROUTER_H
#define ROUTER_H

#include<stdlib.h>

#define TIME_INTERVAL 2

void init_broadcaster(int sockfd);
void receive(int sockfd);

#endif
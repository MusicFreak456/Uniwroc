/* Cezary Świtała 316746 */
#ifndef COMMON_H
#define COMMON_H

#include<stdlib.h>
#include<arpa/inet.h>
#include<stdio.h>
#include<errno.h>

#ifdef DEBUG
#define debug(...) printf(__VA_ARGS__)
#else
#define debug(...)
#endif

#define min(a,b) (a > b) ? b : a

#define error(proc, msg) \
  fprintf(stderr, "%s : %s\n", proc, msg); \
  exit(EXIT_FAILURE);

#define PACKET_SIZE 1000
#define REQ_RES_STR_MAXLEN 20

typedef struct sockaddr_in sockaddr_in;
typedef char* string;

#endif
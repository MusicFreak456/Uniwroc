/* Cezary Świtała 316746 */
#include<iostream>
#include<sys/socket.h>
#include<cerrno>
#include<cstring>
#include<unistd.h>

#include"http_server.hpp"
#include"common.hpp"

int main(int argc, char const *argv[]) {
  if(argc != 3) {
    printf("Usage: %s port working_dir\n", argv[0]);
    return EXIT_FAILURE;
  }

  int status = chdir(argv[2]);
  check_status("chdir", status);

  int port = std::atoi(argv[1]);
  if(port == 0) 
    error("atoi", "invalid port number");

  HttpServer http_server;
  http_server.start(port);
  /* unreachable */
}
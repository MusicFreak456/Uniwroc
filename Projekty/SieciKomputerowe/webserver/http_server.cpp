/* Cezary Świtała 316746 */
#include"http_server.hpp"
#include<sys/socket.h>
#include<arpa/inet.h>
#include<unistd.h>
#include<optional>
#include"common.hpp"
#include"http_repository.hpp"
#include"http_evaluator.hpp"

int initialize_tcp_socket() {
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if(sockfd < 0) {
    error("sockfd", strerror(errno));
  }
  #ifdef DEBUG
  int opt = 1;
  setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(int));
  #endif
  return sockfd;
}

HttpServer::HttpServer() 
  : listening_socket(initialize_tcp_socket()) {
  debug("Server initialized");
}

void HttpServer::bind_to_port(int port) {
  struct sockaddr_in server_address;
  bzero(&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(port);
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);
  int status = 
    bind(listening_socket, (struct sockaddr*)&server_address, 
         sizeof(server_address));
  check_status("bind", status);
}

void HttpServer::start_listening() {
  int status = listen(listening_socket, REQUEST_QUEUE_SIZE);
  check_status("listen", status);
}

int try_accept_connection(int listening_socket) {
  int sockfd = accept(listening_socket, NULL, NULL);
  check_status("accept", sockfd);
  return sockfd;
}

void try_close_connection(int connection_sockfd) {
  int status = close(connection_sockfd);
  check_status("close", status);
}

void handle_requests(int connection_sockfd) {
  using namespace std;
  HttpRepository http_repository(connection_sockfd);

  while (true) {
  try {
    optional<HttpRequest> request = http_repository.receiveRequest();
    if(!request.has_value()) break;
    HttpResponse response = HttpEvaluator::eval_request(request.value());
    http_repository.send_response(response);
    if(request.value().close_requested()) break;
  } catch (const BadRequestException& e) {
    HttpResponse response = HttpEvaluator::bad_request_response();
    http_repository.send_response(response);

  } catch (const NotImplementedException& e) {
    HttpResponse response = HttpEvaluator::not_implemented_response();
    http_repository.send_response(response);
  }}
}

void HttpServer::main_loop() {
  while (true) {
    int connection_sockfd = try_accept_connection(listening_socket);
    debug("Connection established");
    handle_requests(connection_sockfd);
    try_close_connection(connection_sockfd);
    debug("Connection closed");
  }
}

void HttpServer::start(int port) {
  bind_to_port(port);
  start_listening();
  debug("Server started on port " << port);
  main_loop();
}

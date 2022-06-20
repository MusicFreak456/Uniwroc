/* Cezary Świtała 316746 */
#pragma once
#include"http_message.hpp"
#include<optional>
#include<string>

class HttpRepository {
private:
  int connection_sockfd;
public:
  HttpRepository(int connection_sockfd);
  std::optional<HttpRequest> receiveRequest();
  void send_response(HttpResponse &response);
private:
  std::string receive_header();
  void send_buffer(void *buffer, int size);
};

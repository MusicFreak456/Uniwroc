/* Cezary Świtała 316746 */
#pragma once

class HttpServer {
private:
  int listening_socket;
public:
  HttpServer();
  void start(int port);
private:
  void main_loop();
  void bind_to_port(int port);
  void start_listening();
};

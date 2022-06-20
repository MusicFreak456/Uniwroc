/* Cezary Świtała 316746 */
#include"http_repository.hpp"
#include<unistd.h>
#include<sys/socket.h>
#include<string>

#include"common.hpp"
#include"http_mapper.hpp"

class TimeOutException : public std::exception {};
class BufferOverflowException : public std::exception {};
class ClientDisconnectedException : public std::exception {};
class SendingException : public std::exception {};

HttpRepository::HttpRepository(int connection_sockfd) 
  : connection_sockfd(connection_sockfd) {}

int find_header_end(const char *buffer, int bytes_num) {
  static int state = 0;
  for(int i = 0; i < bytes_num; i++) {
    char current_charachter = buffer[i];

    if(current_charachter == '\r') {
      state += state % 2 == 0;
    } else if (current_charachter == '\n') {
      state += state % 2;
    } else {
      state = 0;
    }

    if(state == 4) {
      state = 0;
      return i;
    }
  }
  return -1;
}

void try_select(int sockfd, struct timeval &tv){
  fd_set descriptors;
  FD_ZERO(&descriptors);
  FD_SET(sockfd, &descriptors);
  int status = select(sockfd + 1, &descriptors, NULL, NULL, &tv);
  if(status < 0) error("select", strerror(errno));
  if(status == 0) throw TimeOutException();
}

int try_read(int sockfd, char *buffer, int size) {
  int bytes_read;
  bytes_read = recv(sockfd, buffer, size, 0);
  if(bytes_read == 0) throw ClientDisconnectedException();
  if(bytes_read < 0) error("read", strerror(errno));
  return bytes_read;
}

std::string HttpRepository::receive_header() {
  char buffer[MESSAGE_BUFFER_SIZE];

  struct timeval tv = {
    .tv_sec = CONNECTION_TIMEOUT_S, 
    .tv_usec = CONNECTION_TIMEOUT_US
  };

  int total_bytes_read = 0;
  while(total_bytes_read < MESSAGE_BUFFER_SIZE) {
    try_select(connection_sockfd, tv);
    int bytes_read = try_read(
      connection_sockfd, buffer + total_bytes_read, 
      MESSAGE_BUFFER_SIZE - total_bytes_read
    );

    int end = find_header_end(buffer + total_bytes_read, bytes_read);
    if(end >= 0) {
      buffer[total_bytes_read + end + 1] = '\0';
      return std::string(buffer);
    }

    total_bytes_read += bytes_read;
  }
  throw BufferOverflowException();
} 

std::optional<HttpRequest> HttpRepository::receiveRequest() {
  using namespace std;
  try {
    string header = receive_header();
    HttpRequest request = HttpMapper::request_from_header(header);
    debug("received request");
    return request;
  } catch (const TimeOutException& e) {
    debug("Connection time out");
  } catch (const BufferOverflowException& e) {
    debug("Message buffer full");
  } catch (const ClientDisconnectedException& e) {
    debug("Client disconnected");
  }
  return nullopt;
}

void HttpRepository::send_buffer(void *buffer, int size) {
  char *buff = (char *)buffer;
  int bytes_left = size;
  while (bytes_left > 0) {
    int bytes_sent = send(connection_sockfd, buff, size, 0);
    if(bytes_sent < 0) {
      if(errno == EPIPE) throw ClientDisconnectedException();
      if(errno == EINTR) continue;
      error("write", strerror(errno));
    }
    buff += bytes_sent;
    bytes_left -= bytes_sent;
  }
}

void HttpRepository::send_response(HttpResponse &response) {
  using namespace std;
  try {
    string header = HttpMapper::response_to_header(response);
    send_buffer(header.data(), header.size());
    vector<char> &data = response.get_data();
    send_buffer(data.data(), data.size());
    debug("response sent");
  } catch (ClientDisconnectedException& e) {
    debug("Client disconnected");
  }
}

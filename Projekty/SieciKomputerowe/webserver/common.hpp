/* Cezary Świtała 316746 */
#pragma once
#include<iostream>
#include<cstring>
#include<cerrno>

#ifdef DEBUG
#define debug(X) std::cerr << "[debug] " << X << std::endl
#else
#define debug(X)
#endif

inline void error [[noreturn]] (const char *proc, const char *msg) {
  using namespace std;
  cerr << proc << ": " << msg << endl;
  exit(EXIT_FAILURE);
}

inline void check_status(const char* proc, int status) {
  if(status < 0)
    error(proc, strerror(errno));
}

class BadRequestException : public std::exception {};
class NotImplementedException : public std::exception {};
class ForbiddenException : public std::exception {};

#define REQUEST_QUEUE_SIZE 64
#define CONNECTION_TIMEOUT_S 0
#define CONNECTION_TIMEOUT_US 500000
#define MESSAGE_BUFFER_SIZE 8000

#define PROTOCOL "HTTP/1.1"

#define OK_CODE 200
#define OK_MSG "OK"
#define REDIRECT_CODE 301
#define REDIRECT_MSG "Moved Permanently"
#define BAD_REQUEST_CODE 400
#define BAD_REQUEST_MSG "Bad Request"
#define FORBIDDEN_CODE 403
#define FORBIDDEN_MSG "Forbidden"
#define NOT_FOUND_CODE 404
#define NOT_FOUND_MSG "Not Found"
#define NOT_IMPLEMENTED_CODE 501
#define NOT_IMPLEMENTED_MSG "Not implemented"

/* Cezary Świtała 316746 */
#pragma once
#include<string>
#include<vector>
#include"http_message.hpp"
#include"common.hpp"

class InvalidResponseException : public std::exception {};
class UknownMethodException : public NotImplementedException {};
class UknownProtocolException : public NotImplementedException {};

class HttpMapper {
public:
  static HttpRequest request_from_header(std::string request_str);
  static std::string response_to_header(HttpResponse &response);
};

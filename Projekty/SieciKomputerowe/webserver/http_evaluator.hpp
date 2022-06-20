/* Cezary Świtała 316746 */
#pragma once
#include"http_message.hpp"

class HttpEvaluatorException : public std::exception {};

class HttpEvaluator {
public:
  static HttpResponse eval_request(HttpRequest &request);
  static HttpResponse bad_request_response();
  static HttpResponse not_implemented_response();
};

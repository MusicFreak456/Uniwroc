/* Cezary Świtała 316746 */
#pragma once
#include"http_message.hpp"
#include<string>
#include<filesystem>

class HttpMessageFactory {
public:
  HttpResponse create_not_found_response();
  HttpResponse create_redirect(std::string location);
  HttpResponse create_ok_response(std::filesystem::path resource);
  HttpResponse create_bad_request_response();
  HttpResponse create_not_implemented_response();
  HttpResponse create_forbidden_response();
};

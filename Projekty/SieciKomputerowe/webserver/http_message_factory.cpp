/* Cezary Świtała 316746 */
#include"http_message_factory.hpp"
#include"common.hpp"
#include<sstream>
#include<fstream>
#include"http_type_resolver.hpp"

#include"utils.hpp"

const std::string error_html_template = 
"<!DOCTYPE html>\n"
"<html lang=\"en\">\n"
"  <head>\n"
"    <meta charset=\"utf-8\">\n"
"    <title>Error</title>\n"
"  </head>\n"
"  <body>\n"
"    %d %s\n"
"  </body>\n"
"</html>";

void attach_html(HttpResponse &response, int code, const char msg[]) {
  std::string data = format(error_html_template, code, msg);
  int size = data.size();
  std::stringstream data_stream(data);
  response.data_from_stream(data_stream, size);
  response.set_attribute("Content-Length", std::to_string(data.size()));
}

HttpResponse HttpMessageFactory::create_ok_response(
  std::filesystem::path resource
) {
  using namespace std;
  using namespace filesystem;
  HttpResponse response;
  response.set_status(OK_CODE);

  ifstream file(resource, ios::binary | ios::ate);
  streamsize size = file.tellg();
  file.seekg(0, ios::beg);
  response.data_from_stream(file, size);
  response.set_attribute("Content-Length", std::to_string(size));

  string extension = resource.extension();
  response.set_attribute(
    "Content-Type", HttpTypeResolver::resolve_content_type(extension)
  );

  return response;
}

HttpResponse HttpMessageFactory::create_not_found_response() {
  HttpResponse response;
  response.set_status(NOT_FOUND_CODE);
  attach_html(response, NOT_FOUND_CODE, NOT_FOUND_MSG);
  return response;
}

HttpResponse HttpMessageFactory::create_redirect(std::string location) {
  HttpResponse response;
  response.set_status(REDIRECT_CODE);
  response.set_attribute("Location", location);
  attach_html(response, REDIRECT_CODE, REDIRECT_MSG);
  return response;
}

HttpResponse HttpMessageFactory::create_bad_request_response() {
  HttpResponse response;
  response.set_status(BAD_REQUEST_CODE);
  attach_html(response, BAD_REQUEST_CODE, BAD_REQUEST_MSG);
  return response;
}

HttpResponse HttpMessageFactory::create_not_implemented_response() {
  HttpResponse response;
  response.set_status(NOT_IMPLEMENTED_CODE);
  attach_html(response, NOT_IMPLEMENTED_CODE, NOT_IMPLEMENTED_MSG);
  return response;
}

HttpResponse HttpMessageFactory::create_forbidden_response() {
  HttpResponse response;
  response.set_status(FORBIDDEN_CODE);
  attach_html(response, FORBIDDEN_CODE, FORBIDDEN_MSG);
  return response;
}


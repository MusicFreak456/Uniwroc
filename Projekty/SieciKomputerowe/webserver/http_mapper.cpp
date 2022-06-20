/* Cezary Świtała 316746 */
#include"http_mapper.hpp"
#include<sstream>
#include<iostream>
#include<limits>

#include"common.hpp"

HttpRequest HttpMapper::request_from_header(std::string request_string) {
  using namespace std;
  HttpRequest request;
  istringstream stream(request_string);
  string method;

  stream >> method;

  if(method == "GET") {
    request.set_method(GET);
  } else {
    throw UknownMethodException();
  }

  string url;
  stream >> url;
  request.set_url(url);

  string protocol;
  stream >> protocol;
  if(protocol != PROTOCOL)
    throw UknownProtocolException();

  stream.ignore(1, '\r');
  stream.ignore(1, '\n');

  string attribute_line;
  while (getline(stream, attribute_line, '\r')) {
    if(attribute_line.empty()) break;

    size_t delimiter_position = attribute_line.find(':');
    if(delimiter_position == string::npos) throw BadRequestException();

    string attribute = attribute_line.substr(0, delimiter_position);
    string value = attribute_line.substr(
      delimiter_position + 2, attribute_line.size() - delimiter_position
    );
    
    request.set_attribute(attribute, value);
    
    stream.ignore(1, '\n');
  }

  return request;
}

std::string HttpMapper::response_to_header(HttpResponse &response) {
  using namespace std;
  string header;

  int status = response.get_status();
  header += PROTOCOL " " + to_string(status) + " ";
  switch (response.get_status())
  {
  case OK_CODE:
    header += OK_MSG;
    break;
  case REDIRECT_CODE:
    header += REDIRECT_MSG;
    break;
  case NOT_FOUND_CODE:
    header += NOT_FOUND_MSG;
    break;
  case BAD_REQUEST_CODE:
    header += BAD_REQUEST_MSG;
    break;
  case NOT_IMPLEMENTED_CODE:
    header += NOT_IMPLEMENTED_MSG;
    break;
  case FORBIDDEN_CODE:
    header += FORBIDDEN_MSG;
    break;
  default:
    throw InvalidResponseException();
    break;
  }
  header += "\r\n";

  response.foreach_argument([&header](string name, string value) {
    header += name + ": " + value + "\r\n";
  });

  header += "\r\n";

  return header;
}

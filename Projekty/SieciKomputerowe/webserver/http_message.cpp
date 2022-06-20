/* Cezary Świtała 316746 */
#include"http_message.hpp"
#include<utility>
#include"common.hpp"

HttpMessage::HttpMessage() : attributes() {}

void HttpMessage::set_attribute(std::string attribute, std::string value){
  using namespace std;
  attributes.insert(make_pair(attribute, value));
}

std::optional<std::string> HttpMessage::get_attribute(std::string attribute) {  
  auto search = attributes.find(attribute);
  if(search == attributes.end()) return std::nullopt;
  else return search->second;
}

void HttpMessage::foreach_argument(ArgCallback &&callback) {
  for(auto &field: attributes) {
    callback(field.first, field.second);
  }
}

std::vector<char>& HttpResponse::get_data() {
  return data;
}

HttpRequest::HttpRequest() : HttpMessage() {}

HttpMethod HttpRequest::get_method() { 
  return this->method; 
}

void HttpRequest::set_method(HttpMethod m) { 
  this->method = m; 
}

std::string HttpRequest::get_url() { 
  return this->url;
}

void HttpRequest::set_url(std::string u) { 
  this->url = u;
}

bool HttpRequest::close_requested() {
  using namespace std;
  optional<string> conn_value = get_attribute("Connection");
  if(!conn_value.has_value()) return false;

  string value = conn_value.value();
  transform(value.begin(), value.end(), value.begin(), 
    [](unsigned char c){ return std::tolower(c); });
  
  if(conn_value.value() == "close") return true;
  else return false;
}

HttpResponse::HttpResponse() : HttpMessage(), data() {}

int HttpResponse::get_status() { 
  return this->status; 
}

void HttpResponse::set_status(int s) { 
  this->status = s; 
}

void HttpResponse::data_from_stream(std::istream &file, std::streamsize size){
  data.resize(size);
  if(!file.read(data.data(), size)){
    error("file.read", "Error");
  }
}

/* Cezary Świtała 316746 */
#pragma once

#include<string>
#include<unordered_map>
#include<vector>
#include<fstream>
#include<optional>
#include<functional>

enum HttpMethod {
  GET
};

typedef std::function<void(std::string, std::string)> ArgCallback;

class HttpMessage {
private:
  std::unordered_map<std::string, std::string> attributes;
public:
  HttpMessage();
  void set_attribute(std::string attribute, std::string value);
  std::optional<std::string> get_attribute(std::string attribute);

  void foreach_argument(ArgCallback &&callback);
};

class HttpRequest : public HttpMessage {
private:
  HttpMethod method;
  std::string url;
public:
  HttpRequest();
  void set_method(HttpMethod method);
  HttpMethod get_method();
  void set_url(std::string url);
  std::string get_url();

  bool close_requested();
};

class HttpResponse : public HttpMessage {
private:
  int status;
  std::vector<char> data;
public:
  HttpResponse();
  void set_status(int status);
  int get_status();

  void data_from_stream(std::istream &input_stream, std::streamsize size);
  std::vector<char>& get_data();
};

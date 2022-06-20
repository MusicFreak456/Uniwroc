/* Cezary Świtała 316746 */
#include"http_evaluator.hpp"
#include<string>
#include<optional>
#include"http_message_factory.hpp"

#include"common.hpp"

bool is_forbidden_path(std::filesystem::path resource_path, std::string domain){
  using namespace std;
  using namespace filesystem;

  string cwd_string = current_path();
  cwd_string += "/" + domain + "/";
  int cwd_length = cwd_string.size();

  string resource_path_string(resource_path);
  string resource_path_prefix = resource_path_string.substr(0, cwd_length);
  
  return resource_path_prefix != cwd_string;
}

HttpResponse eval_get_request(HttpRequest &request) {
  using namespace std;
  using namespace filesystem;

  optional<string> maybe_host = request.get_attribute("Host");
  if(!maybe_host.has_value()) throw BadRequestException();
  string host = maybe_host.value();
  string domain = host.substr(0, host.find(':'));

  path resource_path = current_path();
  resource_path.append(domain);
  resource_path.append(request.get_url().erase(0,1));

  HttpMessageFactory message_factory;
  if(!exists(resource_path)) {
    return message_factory.create_not_found_response();

  } else if (is_directory(resource_path)) {
    string location = "http://" + host + request.get_url() + "index.html";
    return message_factory.create_redirect(location);

  } else if(is_forbidden_path(canonical(resource_path), domain)) {
    return message_factory.create_forbidden_response();

  } else if (is_regular_file(resource_path)) {
    return message_factory.create_ok_response(resource_path);

  } else { 
    return message_factory.create_not_found_response();
  }
}

HttpResponse HttpEvaluator::eval_request(HttpRequest &request) {
  switch (request.get_method())
  {
  case GET:
    return eval_get_request(request);
    break;
  default:
    throw NotImplementedException();
    break;
  }
}

HttpResponse HttpEvaluator::bad_request_response() {
  HttpMessageFactory message_factory;
  return message_factory.create_bad_request_response();
}

HttpResponse HttpEvaluator::not_implemented_response() {
  HttpMessageFactory message_factory;
  return message_factory.create_not_implemented_response();
}


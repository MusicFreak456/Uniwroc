/* Cezary Świtała 316746 */
#include"http_type_resolver.hpp"

const std::unordered_map<std::string, std::string> 
  HttpTypeResolver::type_map = {
  {".html", "text/html; charset=utf-8"},
  {".css", "text/css; charset=utf-8"},
  {".txt", "text/plain; charset=utf-8"},
  {".jpg", "image/jpeg"},
  {".jpeg", "image/jpeg"},
  {".png", "image/png"},
  {".pdf", "application/pdf"}
};

std::string HttpTypeResolver::resolve_content_type(std::string extension) {
  auto search = type_map.find(extension);
  if(search == type_map.end()) return "application/octet-stream";
  else return search->second;
}

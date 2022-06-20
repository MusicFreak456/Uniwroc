/* Cezary Świtała 316746 */
#pragma once
#include<string>
#include<unordered_map>

class HttpTypeResolver {
private:
  static const std::unordered_map<std::string, std::string> type_map;
public:
  static std::string resolve_content_type(std::string extension);
};

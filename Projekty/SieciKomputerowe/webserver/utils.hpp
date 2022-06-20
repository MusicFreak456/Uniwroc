/* Cezary Świtała 316746 */
#pragma once
#include<string>

template<typename ... T>
std::string format(const std::string &format, T ... args) {
  using namespace std;
  int result_size = snprintf(nullptr, 0, format.c_str(), args ...) + 1;
  char buffer[result_size];
  snprintf(buffer, result_size, format.c_str(), args ...);
  return string(buffer);
}

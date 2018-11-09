#include "../include/talta/util.hpp"

std::string Talta::Util::toUppercase(std::string input) {
  std::string result;

  for (auto& character: input) {
    result += toupper(character);
  }

  return result;
};

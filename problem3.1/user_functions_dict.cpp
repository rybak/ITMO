#include "user_functions.h"

std::map<std::string, function_t>& user_functions_dict()
{
   static std::map<std::string, function_t> res;
   return res;
}

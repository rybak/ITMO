#ifndef USER_FUNCTIONS_H
#define USER_FUNCTIONS_H
#include <map>
#include <string>
#include "big_int.h"

struct function_t
{
   function_t(){};
   function_t(big_int (*f)(big_int)) : f_(f)
   {}
   big_int operator()(big_int a) const
   {
      return f_(a);
   }
private:
   big_int(*f_)(big_int);
};


std::map<std::string, function_t>& user_functions_dict();

/* void add_function(std::string name, */

#endif

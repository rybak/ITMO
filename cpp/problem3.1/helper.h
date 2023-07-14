#ifndef HELPER_H
#define HELPER_H
#include <boost/spirit/include/phoenix_function.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include "big_int.h"
#include "user_functions.h"

namespace helper
{
   struct construct_big_int_impl
   {
      template<typename T>
      struct result
      {
         typedef big_int type;
      };
      template<typename T>
         big_int operator()(T a) const
      {
         return big_int(a);
      }
   };

   struct power_impl
   {
      template<typename T, typename K>
      struct result
      {
         typedef big_int type;
      };
      template<typename T, typename K>
         big_int operator()(T a, K b) const
      {
         return power(a, b);
      }
   };

   struct user_function_impl
   {
      template<typename T, typename K>
      struct result
      {
         typedef big_int type;
      };
      template<typename T, typename K>
         big_int operator()(T a, K b) const
      {
         return user_functions_dict()[a](b);
      }
   };
}

boost::phoenix::function<helper::construct_big_int_impl>& construct_big_int();
boost::phoenix::function<helper::power_impl>& pow();
boost::phoenix::function<helper::user_function_impl>& user_function();

#endif

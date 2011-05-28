#include "user_functions.h"

big_int sqr(big_int a)
{
   return a * a;
}

big_int fact(big_int a)
{
   static const big_int ZERO(0);
   static const big_int ONE(1);
   static const big_int TWO(2);
   if (a < ZERO)
      throw big_int_factorial_error();
   big_int res = ONE;
   for (big_int i = TWO; i <= a; ++i)
      res *= i;
   return res;
}

big_int digits(big_int a)
{
   static const big_int ZERO(0);
   static const big_int ONE(1);
   static const big_int TEN(10);
   a = abs(a);
   if (a < TEN)
      return ONE;
   big_int len;
   for (big_int tmp(1); tmp <= a; ++len)
      tmp *= 10;
   return len;
}

namespace
{
   struct initializer_
   {
      initializer_()
      {
         user_functions_dict()["sqr"] = function_t(sqr);
         user_functions_dict()["fact"] = function_t(fact);
         user_functions_dict()["digits"] = function_t(digits);
      }
   };
   initializer_ init;
}


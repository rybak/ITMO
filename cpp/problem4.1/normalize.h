#ifndef NORMALIZE_H
#define NORMALIZE_H

#include "big_int.h"
#include "struct_if.h"

namespace
{
   template<typename A, digit_t Carry>
   struct normalize_helper
   {
      typedef typename normalize_helper
      <
         big_int<(A::digit) + (Carry), typename A::tail>,
         0
      >::normalized normalized;
   };

   template<digit_t Carry>
   struct normalize_helper <end_of_big_int, Carry>
   {
      typedef typename normalize_helper
      <
         big_int<Carry, end_of_big_int>,
         0
      >::normalized normalized;
   };

   template<>
   struct normalize_helper<end_of_big_int, 0>
   {
      typedef end_of_big_int normalized;
   };

   template<typename A>
   struct normalize_helper<A, 0>
   {
      typedef big_int
      <
         (A::digit % base),
         typename normalize_helper
         <
            typename A::tail,
            (A::digit / base)
         >::normalized         
      > normalized;
   };

};

template<typename A>
struct normalize
{
   typedef typename normalize_helper<A, 0>::normalized normalized;
};


#endif

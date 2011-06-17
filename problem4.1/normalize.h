#ifndef NORMALIZE_H
#define NORMALIZE_H

#include "big_int.h"

namespace
{
   template<typename A, int Carry>
   struct normalize_helper
   {
      typedef big_int
      <
         (A::digit + Carry) % 10,
         typename normalize_helper
         <
            typename A::tail,
            ((A::digit + Carry) / 10)
         >::normalized
      > normalized;
   };

   template<int Carry>
   struct normalize_helper<end_of_big_int, Carry>
   {
      typedef big_int
      <
         Carry % 10,
         typename normalize_helper
         <
            end_of_big_int,
            Carry / 10
         >::normalized
      > normalized;
   };
   
   template<>
   struct normalize_helper<end_of_big_int, 0>
   {
      typedef end_of_big_int normalized;
   };
}

template<typename A>
struct normalize
{
   typedef typename normalize_helper<A, 0>::normalized normalized;
};

#endif

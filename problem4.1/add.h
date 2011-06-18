#ifndef ADD_H
#define ADD_H

#include "big_int.h"
#include "normalize.h"
#include "reverse.h"

namespace
{
   template<typename A, typename B>
   struct add_helper
   {
      typedef big_int
      <
         (A::digit) + (B::digit),
         typename add_helper
         <
            typename A::tail,
            typename B::tail
         >::sum
      > sum;
   };

   template<typename A>
   struct add_helper<A, end_of_big_int>
   {
      typedef A sum;
   };

   template<typename B>
   struct add_helper<end_of_big_int, B>
   {
      typedef B sum;
   };

   template<>
   struct add_helper<end_of_big_int, end_of_big_int>
   {
      typedef end_of_big_int sum;
   };
}


/* reversed_add takes reversed big_ints and result is reversed add */
template<typename A, typename B>
struct reversed_add
{
   typedef typename normalize
   <
      typename add_helper<A, B>::sum
   >::normalized sum;
};

template<typename A, typename B>
struct add
{
   typedef typename reverse
   <
      typename reversed_add
      <
         typename reverse<A>::reversed,
         typename reverse<B>::reversed
      >::sum
   >::reversed sum;
};

template<typename A>
struct add<A, ZERO>
{
   typedef A sum;
};

template<typename B>
struct add<ZERO, B>
{
   typedef B sum;
};

template<>
struct add<ZERO, ZERO>
{
   typedef ZERO sum;
};

#endif

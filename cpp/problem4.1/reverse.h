#ifndef REVERSE_H
#define REVERSE_H

#include "big_int.h"

namespace
{
   /* struct append output result */
   template<typename A, int Digit>
   struct append
   {
      typedef big_int
      <
         A::digit,
         typename append
         <
            typename A::tail,
            Digit
         >::result
      > result;
   };

   template<int Digit>
   struct append<end_of_big_int, Digit>
   {
      typedef big_int<Digit, end_of_big_int> result;
   };
}

/* struct reverse output reversed */
template<typename A>
struct reverse
{
   typedef typename append
   <
      typename reverse
      <
         typename A::tail
      >::reversed,
      A::digit
   >::result
   reversed;
};

template<>
struct reverse<end_of_big_int>
{
   typedef end_of_big_int reversed;
};

#endif

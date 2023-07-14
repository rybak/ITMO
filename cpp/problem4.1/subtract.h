#ifndef SUBTRACT_H
#define SUBTRACT_H

#include "big_int.h"
#include "reverse.h"
#include "cut_leading_zeros.h"

namespace 
{
   template<typename A, typename B, digit_t Carry>
   struct subtract_helper
   {
      typedef typename struct_if
      <
         (A::digit - B::digit + Carry) < 0,
         big_int
         <
            (A::digit - B::digit + Carry + base),
            typename subtract_helper
            <
               typename A::tail,
               typename B::tail,
               -1
            >::difference
         >,
         big_int
         <
            (A::digit - B::digit + Carry),
            typename subtract_helper
            <
               typename A::tail,
               typename B::tail,
               0
            >::difference
         >
      >::result difference;
   };

   template<typename A, digit_t Carry>
   struct subtract_helper<A, end_of_big_int, Carry>
   {
      typedef typename struct_if
      <
         (A::digit + Carry) < 0,
         big_int
         <
            (A::digit + Carry + base),
            typename subtract_helper
            <
               typename A::tail,
               end_of_big_int,
               -1
            >::difference
         >,
         big_int
         <
            (A::digit + Carry),
            typename A::tail
         >
      >::result difference;
   };

   template<digit_t Carry>
   struct subtract_helper<end_of_big_int, end_of_big_int, Carry>
   {
      typedef end_of_big_int difference;
   };

}

template<typename A, typename B>
struct reversed_subtract
{
   typedef typename reverse
   <
      typename cut_leading_zeros
      <
         typename reverse
         <
            typename subtract_helper<A, B, 0>::difference 
         >::reversed
      >::cut
   >::reversed difference;
};

template<typename A, typename B>
struct subtract
{
   typedef typename cut_leading_zeros
   <
      typename reverse
      <
         typename reversed_subtract
         <
            typename reverse<A>::reversed,
            typename reverse<B>::reversed
         >::difference
      >::reversed
   >::cut difference;
};

template<typename A>
struct subtract<A, ZERO>
{
   typedef A difference;
};

template<>
struct subtract<ZERO, ZERO>
{
   typedef ZERO difference;
};

template<typename A>
struct subtract<A, A>
{
   typedef ZERO difference;
};

#endif

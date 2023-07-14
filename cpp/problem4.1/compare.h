#ifndef COMPARE_H
#define COMPARE_H

#include "reverse.h"
#include "length.h"

/* output result */

namespace
{
   template<typename A, typename B>
   struct compare_equal_lengths
   {
      static const digit_t result =
      struct_if_values
      <
         (A::digit) != (B::digit),
         (A::digit) - (B::digit),
         compare_equal_lengths
         <
            typename A::tail,
            typename B::tail
         >::result
      >::result;
   };

   template<>
   struct compare_equal_lengths<end_of_big_int, end_of_big_int>
   {
      static const digit_t result = 0;
   };

   template<typename A>
   struct compare_equal_lengths<A, end_of_big_int>
   {
      static const digit_t result = 0; /* trash value*/
   };
   template<typename B>
   struct compare_equal_lengths<end_of_big_int, B>
   {
      static const digit_t result = 0; /* trash value*/
   };
   /*********/
   
}

template<typename A, typename B>
struct compare
{
   static const digit_t result = struct_if_values
   <
      (length<A>::len) != (length<B>::len),
      (length<A>::len) - (length<B>::len),
      compare_equal_lengths<A, B>::result
   >::result;
};

template<typename A>
struct compare<A, A>
{
   static const digit_t result = 0;
};

template<typename A, typename B>
struct reversed_compare
{
   static const digit_t result =
   compare
   <
      typename reverse<A>::reversed,
      typename reverse<B>::reversed
   >::result;
};

template<typename A>
struct reversed_compare<A, A>
{
   static const digit_t result = 0;
};

#endif

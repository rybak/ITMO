#ifndef MULTIPLY_H
#define MULTIPLY_H

#include "big_int.h"
#include "reverse.h"
#include "normalize.h"
#include "add.h"

namespace
{
   template<typename A, digit_t B>
   struct multiply_by_digit_helper
   {
      typedef big_int
      <
         ((B) * (A::digit)),
         typename multiply_by_digit_helper
         <
            typename A::tail,
            B
         >::product
      > product;
   };
   
   template<digit_t B>
   struct multiply_by_digit_helper<end_of_big_int, B>
   {
      typedef end_of_big_int product;
   };
}

template<typename A, digit_t B>
struct reversed_multiply_by_digit
{
   typedef typename normalize
   <
      typename multiply_by_digit_helper<A, B>::product
   >::normalized product;
};

template<typename A, digit_t B>
struct multiply_by_digit
{
   typedef typename reverse
   <
      typename reversed_multiply_by_digit
      <
         typename reverse<A>::reversed,
         B
      >::product
   >::reversed product;

};

template<digit_t B>
struct multiply_by_digit<ZERO, B>
{
   typedef ZERO product;
};

template<typename A>
struct multiply_by_digit<A, 0>
{
   typedef ZERO product;
};

template<>
struct multiply_by_digit<ZERO, 0>
{
   typedef ZERO product;
};

template<typename A>
struct multiply_by_digit<A, 1>
{
   typedef A product;
};

/*******************************************************/

namespace
{
   template<typename A, typename B>
   struct multiply_helper
   {
      typedef typename reversed_add
      <
         typename reversed_multiply_by_digit
         <
            A,
            B::digit
         >::product,
         typename multiply_helper
         <
            typename reversed_multiply_by_digit
            <
               A,
               base
            >::product,
            typename B::tail
         >::product
      >::sum product;
   };

   template<typename A>
   struct multiply_helper<A, end_of_big_int>
   {
      typedef end_of_big_int product;
   };
}

template<typename A, typename B>
struct reversed_multiply
{
   typedef typename normalize
   <
      typename multiply_helper<A, B>::product
   >::normalized product;
};

template<typename A, typename B>
struct multiply
{
   typedef typename reverse
   <
      typename reversed_multiply
      <
         typename reverse<A>::reversed,
         typename reverse<B>::reversed
      >::product
   >::reversed product;
};

template<typename A>
struct multiply<A, ZERO>
{
   typedef ZERO product;
};

template<>
struct multiply<ZERO, ZERO>
{
   typedef ZERO product;
};

template<typename A>
struct multiply<ZERO, A>
{
   typedef ZERO product;
};

#endif

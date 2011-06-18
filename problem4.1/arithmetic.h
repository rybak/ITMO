#ifndef ARITHMETIC_H
#define ARITHMETIC_H

#include "big_int.h"
#include "reverse.h"
#include "normalize.h"
#include "cut_leading_zeros.h"

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

/****************************************************************************/
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

   template<typename A>
   struct multiply_by_digit_helper<A, 0>
   {
      typedef ZERO product;
   };

   template<typename A>
   struct multiply_by_digit_helper<A, 1>
   {
      typedef A product;
   };
}

template<typename A, digit_t B>
struct reversed_multiply_by_digit /* A is reversed () */
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

/* FIX multiply_helper */
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
/* FIX multiply_helper */

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

/****************************************************************************/

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

/*********************/

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

/*********************/
/* it is dummy */
template<digit_t Carry>
struct subtract_helper<end_of_big_int, end_of_big_int, Carry>
{
   typedef end_of_big_int difference;
};

/*********************************************************/
template<typename A, typename B>
struct subtract
{
   typedef typename cut_leading_zeros
   <
      typename reverse
      <
         typename subtract_helper
         <
            typename reverse<A>::reversed,
            typename reverse<B>::reversed,
            0
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

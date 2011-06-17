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
private:
   typedef typename reverse<A>::reversed a;
   typedef typename reverse<B>::reversed b;
public:
   typedef typename reverse
   <
      typename reversed_add<a, b>::sum
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

/*********************/

/*********************/


namespace
{
   template<typename A, typename B>
   struct subtract_helper
   {
      typedef big_int
      <
         (A::digit) - (B::digit),
         typename subtract_helper
         <
            typename A::tail,
            typename B::tail
         >::difference
      > difference;
   };

   template<typename A>
   struct subtract_helper<A, end_of_big_int>
   {
      typedef A difference;
   };

   template<typename B>
   struct subtract_helper<end_of_big_int, B>
   {
      typedef typename negate<B>::negative_number difference;
   };

   template<>
   struct subtract_helper<end_of_big_int, end_of_big_int>
   {
      typedef end_of_big_int difference;
   };
}

template<typename A, typename B>
struct reversed_subtract
{
   typedef typename normalize
   <
      typename subtract_helper<A, B>::difference
   >::normalized difference;
};

template<typename A, typename B>
struct subtract
{
public:
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

/**********************/

template<typename A>
struct subtract<A, ZERO>
{
   typedef A difference;
};

template<typename B>
struct subtract<ZERO, B>
{
   typedef typename negate<B>::negative_number difference;
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

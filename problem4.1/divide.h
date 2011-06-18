#ifndef DIVIDE_H
#define DIVIDE_H


#include "big_int.h"
#include "reverse.h"
#include "cut_leading_zeros.h"
#include "struct_if.h"
#include "compare.h"
#include "multiply.h"
#include "add.h"
#include "subtract.h"

namespace
{
   template<typename A, digit_t Carry>
   struct half_helper
   {
      typedef typename struct_if
      <
         ((A::digit + Carry * base) % 2 == 1),
         big_int
         <
            ((A::digit + Carry * base) / 2),
            typename half_helper
            <
               typename A::tail,
               1
            >::result
         >,
         big_int
         <
            ((A::digit + Carry * base) / 2),
            typename half_helper
            <
               typename A::tail,
               0
            >::result
         >
      >::result result;
   };
   
   template<digit_t Carry>
   struct half_helper<end_of_big_int, Carry>
   {
      typedef end_of_big_int result;
   };
}

/*takes reversed big_int and output reversed half*/
template<typename A>
struct reversed_half
{
   typedef typename reverse
   <
      typename cut_leading_zeros
      <
         typename half_helper
         <
            typename reverse<A>::reversed,
            0
         >::result
      >::cut
   >::reversed result;
};

template<typename A>
struct half
{
   typedef typename cut_leading_zeros
   <
      typename half_helper<A, 0>::result
   >::cut result;
};

/********************/
namespace
{
   template<typename L, typename R>
   struct center
   {
      typedef typename half<typename add<L, R>::sum>::result result;
   };

   template<typename A, typename B, typename L, typename C, typename R>
   struct divide_helper
   {
      typedef typename struct_if
      <
         (compare<typename multiply<B, C>::product, A>::result > 0),
         typename divide_helper<A, B, L, typename center<L, C>::result, C>::quotient,
         typename divide_helper<A, B, C, typename center<C, R>::result, R>::quotient
      >::result quotient;
   };
   
   /* L==R-1  =>  (L+R)/2==L */
   template<typename A, typename B, typename L, typename R>
   struct divide_helper<A, B, L, L, R>
   {
      typedef L quotient;
   };

}

template<typename A, typename B>
struct divide
{
private:
   typedef typename add<A, ONE>::sum R;
public:
   typedef typename divide_helper
   <
      A,
      B,
      ZERO,
      typename half<R>::result,
      R
   >::quotient quotient;

   typedef typename subtract
   <
      A,
      typename multiply
      <
         B,
         quotient
      >::product
   >::difference remainder;
};

template<typename A>
struct divide<A, A>
{
   typedef ONE quotient;
   typedef ZERO remainder;
};



#endif

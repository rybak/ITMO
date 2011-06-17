#ifndef SUM_H
#define SUM_H


#include "big_int.h"
#include "reverse.h"
#include "normalize.h"


namespace
{
   template<typename A, typename B>
   struct sum_helper
   {
      typedef big_int
      <
         (A::digit) + (B::digit),
         typename sum_helper
         <
            typename A::tail,
            typename B::tail
         >::sum
      > sum;
   };

   template<typename A>
   struct sum_helper<A, end_of_big_int>
   {
      typedef A sum;
   };

   template<typename B>
   struct sum_helper<end_of_big_int, B>
   {
      typedef B sum;
   };

   template<>
   struct sum_helper<end_of_big_int, end_of_big_int>
   {
      typedef end_of_big_int sum;
   };
}

template<typename A, typename B>
struct sum
{
private:
   typedef typename reverse<A>::reversed a;
   typedef typename reverse<B>::reversed b;
   typedef typename sum_helper<a, b>::sum Sum;
public:
   typedef typename reverse
   <
      typename normalize<Sum>::normalized
   >::reversed result;
};

#endif

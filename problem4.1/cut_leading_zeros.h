#ifndef CUT_LEADING_ZEROS_H
#define CUT_LEADING_ZEROS_H

#include "big_int.h"
#include "struct_if.h"

template<typename A>
struct cut_leading_zeros
{
private:
   //static const digit_t digit = ;
public:
   typedef typename struct_if
   <
      (A::digit == 0),
      typename cut_leading_zeros<typename A::tail>::cut,
      A
   >::result cut;
};

template<>
struct cut_leading_zeros<end_of_big_int>
{
   typedef ZERO cut;
};
#endif

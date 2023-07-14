#ifndef LENGTH_H
#define LENGTH_H

#include "big_int.h"

template<typename A>
struct length
{
   static const digit_t len = 1 + length<typename A::tail>::len;
};

template<>
struct length<end_of_big_int>
{
   static const digit_t len = 0;
};

#endif

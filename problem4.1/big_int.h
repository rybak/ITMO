#ifndef BIG_INT_H
#define BIG_INT_H

#include <fstream>

typedef int digit_t;

static const digit_t base = 10;

struct end_of_big_int
{};

template<digit_t Digit, typename Tail>
struct big_int
{
   static const digit_t digit = Digit;
   typedef Tail tail;
};

typedef big_int<0, end_of_big_int> ZERO;
typedef big_int<1, end_of_big_int> ONE;

/*
template<typename A>
struct negate
{
   typedef big_int
   <
      -A::digit,
      typename negate<typename A::tail>::negative_number
   > negative_number;
};

template<>
struct negate<end_of_big_int>
{
   typedef end_of_big_int negative_number;
};

template<>
struct negate<ZERO>
{
   typedef ZERO negative_number;
};

namespace
{
   template<typename A>
   void print_unsigned_big_int(std::ofstream& out)
   {
      out << std::abs(A::digit);
      //out << A::digit;
      print_unsigned_big_int<typename A::tail>(out);
   }

   template<>
   void print_unsigned_big_int<end_of_big_int>(std::ofstream& out)
   {}
}
*/

template<typename A>
void print_big_int(std::ofstream& out)
{
   /*if (A::digit < 0)
      out << "-";
   print_unsigned_big_int<A>(out);
   */
   //  out << static_case<int>(A::digit);
   //  out << (A::digit);
   out << ((int) (A::digit));
   print_big_int<typename A::tail>(out);
}

template<>
void print_big_int<end_of_big_int>(std::ofstream& out)
{
}

template<typename A>
void println_big_int(std::ofstream& out)
{
   print_big_int<A>(out);
   out << "\n";
}

#endif

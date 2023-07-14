#ifndef BIG_INT_H
#define BIG_INT_H

#include <fstream>

typedef int digit_t;

static const digit_t base = 10;
static const digit_t base_length = 1;

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


namespace
{
   void print_one_digit(std::ofstream& out, digit_t n)
   {
      out << n;
   }
   
   template<typename A>
   void print_big_int_tail(std::ofstream& out)
   {
      print_one_digit(out, A::digit);
      print_big_int_tail<typename A::tail>(out);
   }

   template<>
   void print_big_int_tail<end_of_big_int>(std::ofstream& out)
   {
   }
}
template<typename A>
void print_big_int(std::ofstream& out)
{
   out << A::digit;
   print_big_int_tail<typename A::tail>(out);
}

template<typename A>
void println_big_int(std::ofstream& out)
{
   print_big_int<A>(out);
   out << "\n";
}

#endif

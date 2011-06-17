#ifndef BIG_INT_H
#define BIG_INT_H

#include <fstream>

struct end_of_big_int
{};

template<int Digit, typename Tail>
struct big_int
{
   static const int digit = Digit;
   typedef Tail tail;
};

template<typename A>
void print_big_int(std::ofstream& out)
{
   out << A::digit;
   print_big_int<A::tail>(out);
}

template<>
void print_big_int<end_of_big_int>(std::ofstream& out)
{}

template<typename A>
void println_big_int(std::ofstream& out)
{
   out << A::digit;
   println_big_int<A::tail>(out);
}

template<>
void println_big_int<end_of_big_int>(std::ofstream& out)
{
   out << "\n";
}



#endif

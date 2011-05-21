#ifndef BIG_INT_H
#define BIG_INT_H

//typedef unsigned long size_t;

/*
rybak andrey
big_int.h with    digits_container
*/

#include <string>

#include "digits_container.h"

const long long base = 1000000000;
const size_t base_length = 9;
struct big_int_division_by_zero{};
struct big_int
{
   big_int();
   explicit big_int(long long n);
   big_int& operator=(const big_int&);
   void swap(big_int&);

   friend std::ostream& operator<< (std::ostream&, const big_int&);
   friend std::istream& operator>> (std::istream&, big_int&);
/* compare */
   bool operator>(const big_int&) const;
   bool operator<(const big_int&) const;
   bool operator<=(const big_int&) const;
   bool operator>=(const big_int&) const;
   bool operator==(const big_int&) const;
   bool operator!=(const big_int&) const;
/* arithmetic */
   
   big_int operator-() const;
   
   big_int& operator+=(const big_int&);
   big_int& operator-=(const big_int&);
   big_int& operator*=(long long);
   big_int& operator*=(const big_int&);

   big_int& operator<<=(size_t);
   big_int& operator>>=(size_t);

   std::pair<big_int, big_int> divmod(const big_int&) const;
   big_int& operator/=(const big_int&);
   big_int& operator%=(const big_int&);

   big_int& operator++();
private:
   //typedef vector<long long> digits_container;
   digits_container digits_;

   size_t size() const;
   bool neg_;

   long long compare_to(const big_int&) const;
   long long abs_compare(const big_int&) const;

   void normalize();
};

big_int operator+(const big_int&, const big_int&);
big_int operator-(const big_int&, const big_int&);
big_int operator*(const big_int&, long long);
big_int operator*(long long, const big_int&);
big_int operator*(const big_int&, const big_int&);
big_int operator/(const big_int&, const big_int&);
big_int operator%(const big_int&, const big_int&);

big_int abs(big_int&);

// END big_int.h

#endif

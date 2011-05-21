/*
rybak andrey
big_int.h WITHOUT digits_container
*/

#pragma once

#include <vector>
#include <string>

//#include "digits_container.h"


struct big_int_division_by_zero{};
struct big_int
{
   big_int();
   big_int(long long n);
   big_int(const big_int&);
   big_int& operator=(const big_int&);

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

   big_int& operator++();

private:
   static const long long base = 1000000000;
   static const size_t base_length = 9;
   typedef std::vector<long long> digits_container;
   digits_container digits_;

   size_t size() const;
   bool neg_;

   int compare_to(const big_int&) const;
   int abs_compare(const big_int&) const;

   void normalize();
};

big_int operator+(const big_int&, const big_int&);
big_int operator-(const big_int&, const big_int&);
big_int operator*(const big_int&, long long);
big_int operator*(const big_int&, const big_int&);
big_int operator/(const big_int&, const big_int&);

// END big_int.h

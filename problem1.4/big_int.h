// big_int.h

#pragma once

#include <vector>
#include <string>
using std::vector;
using std::istream;
using std::ostream;

const long long base = 1000000000;
const size_t base_length = 9;

struct big_int_t
{
   big_int_t();
   big_int_t(const big_int_t&);
   big_int_t& operator=(const big_int_t&);
//   const big_int_t& operator+ (big_int_t&) const;

   friend ostream& operator<< (ostream&, const big_int_t&);
   friend istream& operator>> (istream&, big_int_t&);
/* compare */
   bool operator>(const big_int_t&) const;
   bool operator<(const big_int_t&) const;
   bool operator<=(const big_int_t&) const;
   bool operator>=(const big_int_t&) const;
   bool operator==(const big_int_t&) const;
   bool operator!=(const big_int_t&) const;
/* arithmetic */
   
   big_int_t operator-() const;
   
   big_int_t& operator+=(const big_int_t&);
   big_int_t& operator-=(const big_int_t&);


private:
   std::string to_debug_string() const;
   vector<long long> digits_;
   //size_t size();
   size_t size() const;
   bool neg_;
   int compare_to (const big_int_t&) const;
   int abs_compare (const big_int_t&) const;
   void norm();

};

big_int_t operator+(const big_int_t&, const big_int_t&);
big_int_t operator-(const big_int_t&, const big_int_t&);

// END big_int.h

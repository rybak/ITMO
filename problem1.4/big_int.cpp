/*
rybak andrey
big_int.cpp WITHOUT digits_container
*/

#include <string>
#include <fstream>
#include <iostream>

#include "big_int.h"

big_int::big_int(long long n) : negative_(n < 0)
{
   if (negative_)
      n = -n;
   do
   {
      digits_.push_back(n % base);
      n /= base;
   } while (n > 0);
   cut_leading_zeros();
}

big_int::big_int(const big_int& b) : digits_(b.digits_), negative_(b.negative_)
{}

big_int& big_int::operator=(const big_int& b)
{
   digits_ = b.digits_;
   negative_ = b.negative_;
   return *this;
}

big_int big_int::operator-() const
{
   big_int t = *this;
   t.negative_ = !t.negative_;
   return t;
}

big_int& big_int::operator+=(const big_int& b)
{
   if (b.negative_ != negative_)
      return *this -= (-b);
   size_t max_size = std::max(size(), b.size());
   digits_.resize(max_size + 1, 0);
   for (size_t i = 0, n = b.size(); i < n; ++i)
   {
      digits_[i] += b.digits_[i];
      if (digits_[i] >= base)
      {
         digits_[i] -= base;
         digits_[i + 1]++;
      }
   }
   for (size_t i = b.size(); i < max_size; ++i)
   {
      if (digits_[i] >= base)
      {
         digits_[i] -= base;
         digits_[i + 1]++;
      }
   }
   cut_leading_zeros();
   return *this;
}

big_int& big_int::operator++()
{
   return (*this) += big_int(1);
}

big_int& big_int::operator-=(const big_int& b)
{
   if (b.negative_ != negative_)
      return *this += (-b);
   if (b.abs_compare(*this) > 0)
      return *this = -(b - *this);
   size_t max_size = size();
   if (b.size() > max_size)
      max_size = b.size();
   digits_.resize(max_size + 1, 0);
   for (size_t i = 0; i < b.size(); ++i)
   {
      digits_[i] = digits_[i] - b.digits_[i];
      if (digits_[i] < 0)
      {
         digits_[i] += base;
         digits_[i + 1]--;
      }
   }
   for (size_t i = b.size(); i < max_size; ++i)
   {
      if (digits_[i] < 0)
      {
         digits_[i] += base;
         digits_[i + 1]--;
      }
   }
   cut_leading_zeros();
   return *this;
}

big_int& big_int::operator*=(long long b)
{
   if (b >= base)
      return *this *= big_int(b);
   if (b == 0)
   {
      digits_.resize(1);
      digits_[0] = 0;
      negative_ = false;
      return *this;
   }
   if (b < 0)
   {
      b = -b;
      negative_ = !negative_;
   }
   digits_.resize(size() + 1, 0);
   for (size_t i = 0, n = size(); i < n; ++i)
      digits_[i] *= b;
   for (size_t i = 0, n = size(); i < n; ++i)
   {
      if (digits_[i] >= base)
      {
         if ((i + 1) >= n)
            digits_.push_back(digits_[i] / base);
         else
            digits_[i + 1] += digits_[i] / base;
         digits_[i] %= base;
      }
   }
   cut_leading_zeros();
   return *this;
}

big_int& big_int::operator*=(const big_int& b)
{
   big_int c;
   c.negative_ = negative_ ^ b.negative_;
   c.digits_.resize(size() + b.size(), 0);
   size_t pos;
   long long carry;
   size_t n = b.size(), m = size();
   for (size_t i = 0; i < n; ++i)
   {
      carry = 0;
      for (size_t j = 0; j < m; ++j)
      {
         pos = j + i;
         c.digits_[pos] += digits_[j] * b.digits_[i] + carry;
         carry = c.digits_[pos] / base;
         if (c.digits_[pos] > base)
            c.digits_[pos] %= base;
      }
      c.digits_[m + i] += carry;
   }
   c.cut_leading_zeros();
   return *this = c;
}

big_int& big_int::operator<<=(size_t shift)
{
   digits_.resize(size() + (1LL << shift) / base + 1, 0);
   for (size_t j = 0; j < shift; ++j)
   {
      for (size_t i = 0, n = size(); i < n; ++i)
         digits_[i] <<= 1;
      for (size_t i = 0, n = size(); i < n; ++i)
      {
         if (digits_[i] >= base)
         {  
            if ((i + 1) >= n)
               digits_.push_back(digits_[i] / base);
            else
               digits_[i + 1] += digits_[i] / base;
            digits_[i] %= base;
         }
      }
   }
   cut_leading_zeros();
   return *this;
}

big_int& big_int::operator>>=(size_t shift)
{
   long long carry;
   
   for (size_t i = 0; i < shift; i++)
   {
      digits_[0] >>= 1;
      for (size_t j = 1, n = size(); j < n; ++j)
      {
         carry = 1 & digits_[j];
         digits_[j] >>= 1;
         if (carry)
            digits_[j - 1] += (base >> 1);
      }
   }
   cut_leading_zeros();
   return *this;
}

std::pair<big_int, big_int> big_int::divmod(const big_int& b) const
{
   big_int dividend(*this);
   big_int divisor(b);
   /* absolute division
      signs will be given later*/
   dividend.negative_ = false;
   divisor.negative_ = false;
   
   if (divisor == big_int(0))
   {
      throw big_int_division_by_zero();
   }

   big_int quotient(0);
   quotient.digits_.resize(dividend.size(), 0);

   long long shift(0);
   while (divisor < dividend)
   {
      divisor <<= 1;
      shift++;
   }
   if (divisor > dividend)
   {
      divisor >>= 1;
      shift--;
   }
   if (shift >= 0)
   {
      for(long i = 0; i <= shift; i++)
      {
         if (divisor <= dividend)
         {          
            dividend -= divisor;
            divisor  >>= 1;
            quotient <<= 1;
            ++quotient;
         }
         else
         {
            divisor >>= 1;
            quotient <<= 1;
         }
      }
   }
   quotient.negative_ = negative_ ^ b.negative_;
   dividend.negative_ = negative_;
   quotient.cut_leading_zeros();
   dividend.cut_leading_zeros();
   return std::make_pair(quotient, dividend);
}

big_int operator+(const big_int&a, const big_int& b)
{
   big_int t = a;
   t += b;
   return t;
}

big_int operator-(const big_int&a, const big_int& b)
{
   big_int t = a;
   t -= b;
   return t;
}

big_int operator*(const big_int&a, long long b)
{
   big_int t = a;
   t *= b;
   return t;
}

big_int operator*(const big_int&a, const big_int& b)
{
   big_int t = a;
   t *= b;
   return t;
}

int big_int::abs_compare(const big_int& b) const
{
   if (size() != b.size())
      return (size() > b.size())? 1 : -1;
   for (long i = size() - 1; i >= 0; --i)
      if (digits_[i] != b.digits_[i])
      {
         if (digits_[i] < b.digits_[i])
            return -1;
         else
            return 1;
      }
   return 0;
}

int big_int::compare_to(const big_int& b)const
{
   if (this->negative_)
   {
      if (b.negative_)
         return (-b).compare_to(-(*this));
      else
         return -1;
   }
   else
      if (b.negative_)
         return 1;
   return abs_compare(b);
}

bool big_int::operator>(const big_int& b)const
{
   return this->compare_to(b) > 0;
}

bool big_int::operator<(const big_int& b)const
{
   return this->compare_to(b) < 0;
}

bool big_int::operator>=(const big_int& b)const
{
   return this->compare_to(b) >= 0;
}

bool big_int::operator<=(const big_int& b)const
{
   return this->compare_to(b) <= 0;
}

bool big_int::operator==(const big_int& b)const
{
   return this->compare_to(b) == 0;
}

bool big_int::operator!=(const big_int& b)const
{
   return this->compare_to(b) != 0;
}

std::ostream& operator<<(std::ostream& stream, const big_int& var)
{
   if (var.negative_)
      stream << '-';
   stream << var.digits_[var.size() - 1];
   for (size_t i = 0, n = var.size() - 1; i < n; ++i)
   {
      std::string s;
      size_t l = 0;
      for (long long temp = var.digits_[n - i - 1]; temp > 9; )
      {
         temp /= 10;
         l++;
      }
      for (size_t j = l + 1; j < big_int::base_length; ++j)
         stream << '0';
      stream << var.digits_[n - i - 1];
   }

   return stream;
}

std::istream& operator>>(std::istream& stream, big_int& var)
{
   while (!stream.eof() && isspace(stream.peek()))
      stream.get();
   if (stream.eof())
   {
      stream.setstate(std::ios::failbit);
      return stream;
   }
   std::string s = "";
   if (stream.peek() == '+' || stream.peek() == '-') {
      if (stream.peek() == '-')
         s.push_back('-');
      stream.get();
   }
   if (stream.eof())
   {
      stream.setstate(std::ios::failbit);
      return stream;
   }
   if (!isdigit(stream.peek()))
   {
      stream.seekg(0, std::ios::end);
      stream.setstate(std::ios::failbit);
      return stream;
   }
   while (!stream.eof() && isdigit(stream.peek()))
   {
      s.push_back(static_cast<char>(stream.get()));
   }
   
   size_t len = s.length();

   if (len == 0)
      return stream;

   size_t start_pos = 0;
   if (s[0] == '-')
   {
      start_pos = 1;
      len--;
      var.negative_ = true;
   }
   else
      var.negative_ = false;
   var.digits_.assign((len - 1) / big_int::base_length + 1, 0);
  
   size_t first_digit_len = len - (var.size() - 1) * big_int::base_length;
   for (size_t j = start_pos; j < (first_digit_len + start_pos); ++j)
      var.digits_[var.size() - 1] = var.digits_[var.size() - 1] * 10 + s[j] - '0';
   
   first_digit_len += start_pos;
   for (size_t i = 0, n = var.size() - 1; i < n; ++i)
   {
      size_t pos = first_digit_len + (i * big_int::base_length);
      for (size_t j = pos; j < (pos + big_int::base_length); ++j)
         var.digits_[n - i - 1] = var.digits_[n - i - 1] * 10 + s[j] - '0';
   }
   return stream;
}

size_t big_int::size() const
{
   return digits_.size();
}

void big_int::cut_leading_zeros()
{
   while ((digits_.size() > 1) && (digits_.back() == 0))
      digits_.pop_back();
   if (digits_[0] == 0)
      if (digits_.size() == 1)
         negative_ = false;
}

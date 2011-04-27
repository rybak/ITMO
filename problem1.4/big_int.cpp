#pragma once
#include "big_int.h"
#include <string>
#include <fstream>
#include <iostream>
using std::string;
big_int_t::big_int_t()
{
   digits_.push_back(0);
   neg_ = false;
}

big_int_t::big_int_t(const big_int_t& b)
{
   digits_ = b.digits_;
   neg_ = b.neg_;
}

big_int_t big_int_t::operator-() const
{
   big_int_t t = *this;
   t.neg_ = !t.neg_;
   return t;
}

big_int_t& big_int_t::operator+=(const big_int_t& b)
{
//   std::cerr << "\noperator+=\n" << *this << "\n" << b << "\n";
   if (b.neg_ != neg_)
      return *this -= (-b);
   size_t max_size = size();
   if (b.size() > max_size)
      max_size = b.size();
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
   while (digits_.back() == 0)
      digits_.pop_back();
   return *this;
}


big_int_t& big_int_t::operator-=(const big_int_t& b)
{
  // std::cerr << "\noperator-=\n" << *this << "\n" << b << "\n";
   //std::ofstream debug_out("debug", std::ios_base::app);
   if (b.neg_ != neg_)
      return *this += (-b);
   if (b.abs_compare(*this) > 0)
   {
      big_int_t t = b;
      t -= *this;
      return *this = -t;
   }
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
   while (digits_.back() == 0)
      digits_.pop_back();
   return *this;
}


/*
void big_int_t::divmod(big_int_t& digits_, big_int_t& b, big_int_t& div, big_int_t& mod)
{

}
*/
/*big_int_t to_big_int_t_from_()*/

big_int_t operator+(const big_int_t&a, const big_int_t& b)
{
   big_int_t t = a;
   t += b;
   return t;
}

big_int_t operator-(const big_int_t&a, const big_int_t& b)
{
   big_int_t t = a;
   t -= b;
   return t;
}

// compare

int big_int_t::abs_compare(const big_int_t& b) const
{
   if (size() != b.size())
      return (size() > b.size())? 1 : 0;
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

int big_int_t::compare_to(const big_int_t& b)const
{
   if (this->neg_)
   {
      if (b.neg_)
         return (-b).compare_to(-(*this));
      else
         return -1;
   }
   else
      if (b.neg_)
         return 1;
   if (size() != b.size())
      return (size() > b.size())? 1 : 0;
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

bool big_int_t::operator>(const big_int_t& b)const
{
   return this->compare_to(b) > 0;
}

bool big_int_t::operator<(const big_int_t& b)const
{
   return this->compare_to(b) < 0;
}

bool big_int_t::operator>=(const big_int_t& b)const
{
   return this->compare_to(b) >= 0;
}

bool big_int_t::operator<=(const big_int_t& b)const
{
   return this->compare_to(b) <= 0;
}

bool big_int_t::operator==(const big_int_t& b)const
{
   return this->compare_to(b) == 0;
}

bool big_int_t::operator!=(const big_int_t& b)const
{
   return this->compare_to(b) != 0;
}

big_int_t & big_int_t::operator=(const big_int_t& b)
{
   digits_ = b.digits_;
   neg_ = b.neg_;
   return *this;
}

// input output // big_int_t

ostream& operator<<(ostream& stream, const big_int_t& var)
{
   if (var.neg_)
      stream << '-';
   stream << var.digits_[var.size() - 1];
   for (size_t i = 0, n = var.size() - 1; i < n; ++i)
   {
      string s;
      size_t l = 0;
/*      if (var.digits_[n - i - 1] == 0)
      {
         stream << "000000000";
         continue;
      }*/
      for (long long temp = var.digits_[n - i - 1]; temp > 9; )
      {
         temp /= 10;
         l++;
      }
      for (size_t j = l + 1; j < base_length; ++j)
         stream << '0';
      stream << var.digits_[n - i - 1];
   }

   return stream;
}

istream& operator>>(istream& stream, big_int_t& var)
{
   string s;
   stream >> s;
   size_t start_pos = 0;

   size_t len = s.length();

   if (s[0] == '-')
   {
      start_pos = 1;
      len--;
      var.neg_ = true;
   }
   else
      var.neg_ = false;
   
   var.digits_.resize((len - 1) / base_length + 1, 0);
   
   size_t first_digit_len = len - (var.size() - 1) * base_length;
   for (size_t j = start_pos; j < (first_digit_len + start_pos); ++j)
      var.digits_[var.size() - 1] = var.digits_[var.size() - 1] * 10 + s[j] - '0';
   
   first_digit_len += start_pos;

   for (size_t i = 0, n = var.size() - 1; i < n; ++i)
   {
      size_t pos = first_digit_len + (i * base_length);
      for (size_t j = pos; j < (pos + base_length); ++j)
         var.digits_[n - i - 1] = var.digits_[n - i - 1] * 10 + s[j] - '0';
   }

   return stream;
}

size_t big_int_t::size() const
{
   return digits_.size();
}

void big_int_t::norm()
{
   while ((size() > 1) && (digits_.back() == 0))
      digits_.pop_back();
}

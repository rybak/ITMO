#include <assert.h>
#include <algorithm>
#include "digits_container.h"

digits_container::digits_container() : digit_(0), size_(0), capacity_(1)
{}

digits_container::digits_container(size_t count) : size_(count)
{
   if (count <= 1)
   {
      digit_ = 0;
      capacity_ = 1;
      return;
   }
   digits_ = new long long[capacity_ = calc_capacity(count)];
   for (size_t i = 0; i < capacity_; ++i)
      digits_[i] = 0;
}

digits_container::digits_container(const digits_container &val): size_(val.size_)
{
   if (size_ == 1)
   {
      digit_ = val[0];
      capacity_ = 1;
   }
   else
   {
      digits_ = new long long[capacity_ = calc_capacity(size_)];
      std::copy(val.digits_, val.digits_ + size_, digits_);
   }
}

digits_container& digits_container::operator=(const digits_container &val)
{
   resize(val.size_);
   for (size_t i = 0; i < size_; ++i)
      (*this)[i] = val[i];
   return *this;
}

digits_container::~digits_container()
{
   if (capacity_ > 1)
      delete[] digits_;
}

void digits_container::resize(size_t new_size)
{
   if (new_size == size_)
      return;
   if (new_size > capacity_)
   {
      digits_container new_containter(new_size);
      size_t min_size = std::min(size_, new_size);
      for (size_t i = 0; i < min_size; ++i)
         new_containter[i] = (*this)[i];
      swap(new_containter);
      std::fill(digits_ + min_size, digits_ + new_size, 0);
   }
   else
   {
      if (capacity_ > 1)
         std::fill(digits_ + std::min(size_, new_size), digits_ + new_size, 0);
   }
   size_ = new_size;
      /*
   if (new_size == size_)
      return;
   if (new_size <= 1)
   {
      long long old = 0;
      if (size_ > 0)
         old = (*this)[0];
      if (size_ > 1)
         delete[] digits_;
      capacity_ = 1;
      digit_ = old;
   }
   else
   {
      size_t new_capacity = calc_capacity(new_size);
      size_t min_size = std::min(size_, new_size);
      if (new_capacity > capacity_)
      {
         long long *new_digits = new long long[capacity_ = new_capacity];
         for (size_t i = 0; i < min_size; ++i)
            new_digits[i] = (*this)[i];
         if (size_ > 1)
            delete[] digits_;
         std::fill(new_digits + min_size, new_digits + new_size, 0);
         digits_ = new_digits;
      }
      else
      {
         std::fill(digits_ + min_size, digits_ + new_size, 0);
      }
   }
   size_ = new_size;*/
}

void digits_container::swap(digits_container &other)
{
   if (capacity_ != 1)
   {
      if (other.capacity_ != 1)
         std::swap(digits_, other.digits_);
      else
      {
         long long digit = other.digit_;
         other.digits_ = digits_;
         digit_ = digit;
      }
   }
   else
   {
      if (other.capacity_ != 1)
      {
         long long *digits = other.digits_;
         other.digit_ = digit_;
         digits_ = digits;
      }
      else
         std::swap(digit_, other.digit_);
   }
   std::swap(size_, other.size_);
   std::swap(capacity_, other.capacity_);
}

size_t digits_container::calc_capacity(size_t new_size)
{
   size_t new_capacity = 1;
   while (new_capacity < new_size)
      new_capacity *= capacity_factor;
   return new_capacity;
}

void digits_container::push_back(long long x)
{
   resize(size_ + 1);
   (*this)[size_ - 1] = x;
}

void digits_container::pop_back()
{
   if (size_ == 0)
   {
      throw digits_container_empty();
      return;
   }
   resize(size_ - 1);
}

const long long digits_container::operator[](size_t index) const
{
   if (index < size_)
   {
      if (capacity_ == 1)
         return digit_;
      else
         return digits_[index];
   }
   throw digits_container_bad_index();
}

long long& digits_container::operator[](size_t index)
{
   if (index < size_)
   {
      if (capacity_ == 1)
         return digit_;
      else
         return digits_[index];
   }
   throw digits_container_bad_index();
}

const size_t digits_container::size() const
{
   return size_;
}

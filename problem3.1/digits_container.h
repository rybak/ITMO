#ifndef DIGIT_CONTAINER_H
#define DIGIT_CONTAINER_H

// container for digits in big_int

#include <stddef.h>

struct digits_container_bad_index{};
struct digits_container_empty{};

struct digits_container
{
public:
   digits_container();
   digits_container(size_t);
   digits_container(const digits_container &);
   digits_container & operator=(const digits_container &);

   ~digits_container();
   void push_back(long long x);
   void pop_back();
   void resize(size_t new_size);

   void swap(digits_container &);

   const long long operator[](size_t index) const;
   long long& operator[](size_t index);

   const size_t size() const;
private:
   static const size_t capacity_up = 2;
   static const size_t calc_capacity(size_t new_size);
   union
   {
      long long digit_;
      long long *digits_;
   };
   size_t size_;
   size_t capacity_;
};

#endif

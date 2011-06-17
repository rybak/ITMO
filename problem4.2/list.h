#ifndef VECTOR_H
#define VECTOR_H

#include <fstream>
#include "struct_if.h"

/******************************************************************************/

struct empty_list
{};

template<long n, typename Tail>
struct list
{
   static const long value = n;
   typedef Tail tail;
};

/******************************************************************************/
/* struct list_length output value */
template<typename List>
struct list_length
{
   static const int value = 1 + list_length<typename List::tail>::value;
};

template<>
struct list_length<empty_list>
{
   static const int value = 0;
};

/******************************************************************************/
/* struct list_partition_first output list_part */
template<typename List, int n>
struct list_partition_first
{
   typedef
      list
      <
         List::value,
         typename list_partition_first
         <
            typename List::tail,
            n - 1
         >::list_part
      > list_part;
};

template<typename List>
struct list_partition_first<List, 0>
{
   typedef empty_list list_part;
};

template<int n>
struct list_partition_first<empty_list, n>
{
   typedef empty_list list_part;
};

/******************************************************************************/
/* struct list_partition_second output list_part */

template<typename List, int n>
struct list_partition_second
{
   typedef
      typename struct_if
      <
         n < 0,
         empty_list,
         typename list_partition_second
         <
            typename List::tail,
            n - 1
         >::list_part
      >::result list_part;
};

template<typename List>
struct list_partition_second<List, 0>
{
   typedef List list_part;
};

template<int n>
struct list_partition_second<empty_list, n>
{
   typedef empty_list list_part;
};

template<typename List>
void print_list(std::ofstream& out)
{
   out << List::value << " ";
   print_list<typename List::tail>(out);
}

template<>
void print_list<empty_list>(std::ofstream& out)
{
}


#endif

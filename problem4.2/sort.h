#ifndef SORT_H
#define SORT_H

#include "list.h"
#include "struct_if.h"
/******************************************************************************/
/* struct merge output merged_list */

template<typename List1, typename List2>
struct merge
{
   typedef
      typename struct_if
      <
         (List1::value) < (List2::value),
         list
         <
            List1::value,
            typename merge
            <
               typename List1::tail,
               List2
            >::merged_list
         >,
         list
         <
            List2::value,
            typename merge
            <
               List1,
               typename List2::tail
            >::merged_list
         >
      >::result merged_list;
};

template<typename List1>
struct merge<List1, empty_list>
{
   typedef List1 merged_list;
};

template<typename List2>
struct merge<empty_list, List2>
{
   typedef List2 merged_list;
};

/******************************************************************************/
/* struct merge_sort output sorted_list */

template<typename List>
struct merge_sort
{
   static const int length = list_length<List>::value;
   typedef
      typename merge
      <
         typename merge_sort
         <
            typename list_partition_first
            <
               List,
               length / 2
            >::list_part
         >::sorted_list,
         typename merge_sort
         <
            typename list_partition_second
            <
               List,
               length / 2
            >::list_part
         >::sorted_list
      >::merged_list sorted_list;
};

template<>
struct merge_sort<empty_list>
{
   typedef empty_list sorted_list;
};

template<long value>
struct merge_sort<list<value, empty_list> >
{
   typedef list<value, empty_list> sorted_list;

};

/******************************************************************************/


#endif

#include <fstream>
#include "list.h"
#include "sort.h"

typedef
   list
   <
      4,
      list
      <
         3,
         list
         <
            2,
            list
            <
               1,
               empty_list
            >
         >
      >
   > List;
typedef merge_sort<List>::sorted_list Sorted_List;

int main()
{
   std::ofstream output("out.txt");
   print_list<List>(output);
   output << "\n";
   print_list<Sorted_List>(output);
   return 0;
}
#include <fstream>

#include "big_int.h"
#include "sum.h"

int main()
{

   std::ofstream output("out.txt");
   // 11
   typedef big_int<1, 
           big_int<1, end_of_big_int> > a;
   // 23
   typedef big_int<2,
           big_int<3, end_of_big_int> > b;
   // 2
   typedef big_int<2, end_of_big_int> c;
   // 1234
   typedef big_int<1,
           big_int<2,
           big_int<3,
           big_int<4, end_of_big_int> > > > d;
   typedef big_int<1,
           big_int<0,
           big_int<0,
           big_int<0,
           big_int<0,
           big_int<0,
           big_int<0,
           big_int<0,
           big_int<0,
           big_int<0, end_of_big_int> > > > > > > > > > billion;
   
   /*
   // it is working tests:
   // Test #2
   typedef sum<a, d>::result Sumad;
   typedef sum<d, a>::result Sumda;
   typedef sum<c, d>::result Sumcd;
   typedef sum<d, c>::result Sumdc;
   typedef sum<b, d>::result Sumbd;
   typedef sum<d, b>::result Sumdb;
   
   println_big_int<typename sum<billion, typename Sumad>::result>(output);
   print_big_int<Sumad>(output);
   output << "\n";
   print_big_int<Sumda>(output);
   output << "\n";
   print_big_int<Sumcd>(output);
   output << "\n";
   print_big_int<Sumdc>(output);
   output << "\n";
   print_big_int<Sumbd>(output);
   output << "\n";
   print_big_int<Sumdb>(output);
   output << "\n";
   println_big_int<typename sum<billion, a>::result>(output);
   
   /// Test #1
   typedef sum<a, b>::result Sumab;
   typedef sum<a, c>::result Sumac;
   typedef sum<b, c>::result Sumbc;

   typedef sum<b, a>::result Sumba;
   typedef sum<c, a>::result Sumca;
   typedef sum<c, b>::result Sumcb;

   print_big_int<Sumab>(output);
   output << "\n";
   print_big_int<Sumba>(output);
   output << "\n";
   print_big_int<Sumac>(output);
   output << "\n";
   print_big_int<Sumca>(output);
   output << "\n";
   print_big_int<Sumbc>(output);
   output << "\n";
   print_big_int<Sumcb>(output);
   output << "\n";

   */
   return 0;
}

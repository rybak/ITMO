#include <fstream>

#include "big_int.h"
#include "arithmetic.h"

std::ofstream output("out.txt");

// 11
typedef big_int<1,
        big_int<1, end_of_big_int> > a;
// 23
typedef big_int<2,
        big_int<3, end_of_big_int> > b;
// 2
typedef big_int<2, end_of_big_int>  c;
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
void test1()
{/*
   typedef add<a, b>::sum Sumab;
   typedef add<a, c>::sum Sumac;
   typedef add<b, c>::sum Sumbc;

   typedef add<b, a>::sum Sumba;
   typedef add<c, a>::sum Sumca;
   typedef add<c, b>::sum Sumcb;
   
   typedef add<a, d>::sum Sumad;
   typedef add<d, a>::sum Sumda;
   typedef add<c, d>::sum Sumcd;
   typedef add<d, c>::sum Sumdc;
   typedef add<b, d>::sum Sumbd;
   typedef add<d, b>::sum Sumdb;

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
   
   println_big_int<typename add<billion, typename Sumad>::sum>(output);
   println_big_int<typename add<billion, a>::sum>(output);*/
}

void test2()
{
   // a = 11 b = 23 c = 2 d = 1234
   typedef subtract<b, a>::difference ba;
   print_big_int<b>(output);
   output << "-";
   print_big_int<a>(output);
   output << "=";
   println_big_int<ba>(output);/*
   typedef subtract<a, c>::difference ac;
   typedef subtract<b, c>::difference bc;
   typedef subtract<d, a>::difference da;
   typedef subtract<d, b>::difference db;
   typedef subtract<d, c>::difference dc;
   typedef subtract<d, d>::difference dd;*/
/*   print_big_int<a>(output);
   output << "-";
   print_big_int<c>(output);
   output << "=";
   println_big_int<ac>(output);
   print_big_int<b>(output);
   output << "-";
   print_big_int<c>(output);
   output << "=";
   println_big_int<bc>(output);
   print_big_int<d>(output);
   output << "-";
   print_big_int<a>(output);
   output << "=";
   println_big_int<da>(output);
   print_big_int<d>(output);
   output << "-";
   print_big_int<b>(output);
   output << "=";   
   println_big_int<db>(output);
   print_big_int<d>(output);
   output << "-";
   print_big_int<c>(output);
   output << "=";
   println_big_int<dc>(output);
   print_big_int<d>(output);
   output << "-";
   print_big_int<d>(output);
   output << "=";
   println_big_int<dd>(output);
   
   typedef subtract<billion, d>::difference b_d;
   print_big_int<billion>(output);
   output << "-";
   print_big_int<d>(output);
   output << "=";
   println_big_int<b_d>(output);

   typedef subtract<a, b>::difference ab;
   print_big_int<a>(output);
   output << "-";
   print_big_int<b>(output);
   output << "=";
   println_big_int<ab>(output);*/
   
}

void test3()
{

}
int main()
{
   output << "Test #1: add\n";
   test1();
   output << "\nTest #2: subtract\n";
   test2();
   return 0;
}

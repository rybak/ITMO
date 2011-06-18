#include <fstream>

#include "big_int.h"
#include "arithmetics.h"

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
// 6134629179
typedef big_int<6,
        big_int<1,
        big_int<3,
        big_int<4,
        big_int<6,
        big_int<2,
        big_int<9,
        big_int<1,
        big_int<7,
        big_int<9, end_of_big_int> > > > > > > > > > e;
typedef big_int<9,
        big_int<9, end_of_big_int> > f;
typedef big_int<3, end_of_big_int> g;
void test1()
{
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
   output << "11 + 23 = ";
   print_big_int<Sumab>(output);
   output << " = 23 + 11 = ";
   println_big_int<Sumba>(output);

   output << "11 + 2 = ";
   print_big_int<Sumac>(output);
   output << " = 2 + 11 = ";
   println_big_int<Sumca>(output);

   output << "23 + 2 = ";
   print_big_int<Sumbc>(output);
   output << " = 2 + 23 = ";
   println_big_int<Sumcb>(output);

   output << "11 + 1234 = ";
   print_big_int<Sumad>(output);
   output << " = 1234 + 11 = ";
   println_big_int<Sumda>(output);

   output << "2 + 1234 = ";
   print_big_int<Sumcd>(output);
   output << " = 1234 + 2 = ";
   println_big_int<Sumdc>(output);

   output << "23 + 1234 = ";
   print_big_int<Sumbd>(output);
   output << " = 1234 + 23 = ";
   println_big_int<Sumdb>(output);
   
   println_big_int<typename add<billion, Sumad>::sum>(output);
   println_big_int<typename add<billion, a>::sum>(output);
   println_big_int<typename add<billion, e>::sum>(output);
}

void test2()
{
   // a = 11 b = 23 c = 2 d = 1234
   typedef subtract<b, a>::difference ba;
   output << "23 - 11 = ";
   println_big_int<ba>(output);
   typedef subtract<a, c>::difference ac;
   output << "11 - 2 = ";
   println_big_int<ac>(output);
   typedef subtract<b, c>::difference bc;
   output << "23 - 2 = ";
   println_big_int<bc>(output);
   
   typedef subtract<d, a>::difference da;
   output << "1234 - 11 = ";
   println_big_int<da>(output);
   
   typedef subtract<d, b>::difference db;
   output << "1234 - 23 = ";
   println_big_int<db>(output);
   
   typedef subtract<d, c>::difference dc;
   output << "1234 - 2 = ";
   println_big_int<dc>(output);
   
   typedef subtract<d, d>::difference dd;
   output << "1234 - 1234 = ";
   println_big_int<dd>(output);
   
   typedef subtract<billion, d>::difference b_d;
   output << "1000000000 - 1234 = ";
   println_big_int<b_d>(output);
   output << "6134629179 - 1000000000 = ";
   println_big_int<typename subtract<e, billion>::difference>(output);
}

void test3()
{
   typedef multiply_by_digit<a, 1>::product a1;
   output << "11 * 1 = ";
   println_big_int<a1>(output);
   
   typedef multiply_by_digit<b, 2>::product b2;
   output << "23 * 2 = ";
   println_big_int<b2>(output);
   
   typedef multiply_by_digit<a, 9>::product a9;
   output << "11 * 9 = ";
   println_big_int<a9>(output);
   
   typedef multiply_by_digit<b, 9>::product b9;
   output << "23 * 9 = ";
   println_big_int<b9>(output);
   
   typedef multiply_by_digit<e, 9>::product e9;
   output << "6134629179 * 9 = ";
   println_big_int<e9>(output);
   
   typedef multiply_by_digit<f, 9>::product f9;
   output << "99 * 9 = ";
   println_big_int<f9>(output);

   typedef multiply_by_digit<f, 10>::product f10;
   output << "99 * 10 = ";
   println_big_int<f10>(output);
}

void test4()
{
   // a = 11 b = 23 c = 2 d = 1234 e = 6134629179 f = 99
   typedef multiply<a, b>::product ab;
   output << "11 * 23 = ";
   println_big_int<ab>(output);

   typedef multiply<b, a>::product ba;
   output << "23 * 11 = ";
   println_big_int<ba>(output);

   typedef multiply<e, f>::product ef;
   output << "6134629179 * 99 = ";
   println_big_int<ef>(output);

}

void test5()
{
   // a = 11 b = 23 c = 2 d = 1234 e = 6134629179 f = 99
   typedef half<b>::result hb;
   output << "half 23 = ";
   println_big_int<hb>(output);

   typedef half<e>::result he;
   output << "half 6134629179 = ";
   println_big_int<he>(output);

}
int main()
{
   output << "Test #1: add\n";
   test1();
   output << "\nTest #2: subtract\n";
   test2();
   output << "\nTest #3: multiply_by_digit\n";
   test3();
   output << "\nTest #4: multiply\n";
   test4();
   output << "\nTest #5: half\n";
   test5();
   return 0;
}

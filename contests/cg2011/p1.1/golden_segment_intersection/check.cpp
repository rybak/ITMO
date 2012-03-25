#include <iostream>
#include <memory>

#include <CGAL/Segment_2_Segment_2_intersection.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>

#include "common.h"

#pragma pack(push, 1)
struct segment_pair
{
   cg::segment_2 first;
   cg::segment_2 second;
};
#pragma pack(pop)

typedef CGAL::Exact_predicates_inexact_constructions_kernel CGAL_Kernel;

CGAL_Kernel::Point_2 construct(cg::point_2 const & p)
{
   return CGAL_Kernel::Point_2(p.x, p.y);
}

CGAL_Kernel::Point_2 construct(double x, double y)
{
   return CGAL_Kernel::Point_2(x, y);
}

CGAL_Kernel::Segment_2 construct(cg::segment_2 const & s)
{
   return CGAL_Kernel::Segment_2(construct(s.p0), construct(s.p1));
}

CGAL_Kernel::Segment_2 construct(double x1, double y1, double x2, double y2)
{
   return CGAL_Kernel::Segment_2(construct(x1, y1), construct(x2, y2));
}

using namespace std;

int seed;

struct TestInput 
{
   virtual int get_int() = 0;
   virtual double get_double() = 0;
   virtual ~TestInput() {};
};

class RandomInput: public TestInput
{
   int seed;

   static const int a = 1664525;
   static const int c = 1013904223;
public:
   RandomInput(int seed) 
      :seed(seed)
   { }

   // return random integer in range 0 .. 2^31-1 
   int get_int() 
   {
      return (seed = seed * a + c) & 0x7fffffff;
   }

   double get_double() 
   {
      const double lo = -1000;
      const double hi = 1000;
      const double scale = 1. / (1 << 30);
      return (lo + (hi - lo) * scale * (get_int() & 0x3fffffff));
   }

   ~RandomInput() {}
};

class FileInput: public TestInput
{
   ifstream in;
   int tokens_count;
public:
   FileInput(const char * name) 
      :in(name), tokens_count(0)
   { }

   int get_int() 
   {
      int res;
      in >> res;

      if (!in) 
      {
         if (in.eof()) {
            throw runtime_error("Unexpected end of file");
         }
         throw runtime_error("Can't read an integer");
      }

      return res;
   }

   double get_double() 
   {
      double res;
      in >> res;

      if (!in) 
      {
         if (in.eof()) {
            throw runtime_error("Unexpected end of file");
         }
         throw runtime_error("Can't read a double");
      }

      return res;
   }

   ~FileInput() {}
};

void my_assert(bool p, char const * msg)
{
   if (!p) 
   {
      throw runtime_error(msg);
   }
}


int main(int argc, char ** argv)
{   
   try {

      auto_ptr<TestInput> in(new FileInput(argv[1]));
      int n = in->get_int();

      int type = in->get_int();
      my_assert(type == 0 || type == 1, "Unexpected value of test type");

      if (type == 1) {
         int seed = in->get_int();
         in = auto_ptr<TestInput>(new RandomInput(seed));
      }

      int res = 0;
      int p = 1;
      for (int i = 0; i < n; ++i, p *= 239)
      {
         double x1 = in->get_double();
         double y1 = in->get_double();
         double x2 = in->get_double();
         double y2 = in->get_double();
         double x3 = in->get_double();
         double y3 = in->get_double();
         double x4 = in->get_double();
         double y4 = in->get_double();
         if (CGAL::do_intersect(
               construct(x1, y1, x2, y2),
               construct(x3, y3, x4, y4))) 
         {
            res += p;
         }

      }

      if (argc > 2) 
      {
         FileInput f(argv[2]);
         int res2 = f.get_int();
         if (res != res2) 
         {
            cout << "WA: Expected " << res << ", found " << res2 << "\n";
            return -1;
         }
      }
      return 0;
   } catch (const runtime_error& e) {
      cerr << e.what();
      return -1;
   }
}

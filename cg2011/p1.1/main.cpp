#include <vector>
//#include <fstream>
#include <iostream>
#include "geometry.h"

#include <memory>
//#include "common.h"

/* copy-paste from sample */

class random_input
{
   int seed;
   static const int a = 1664525;
   static const int c = 1013904223;
public:
   random_input(int seed) 
      :seed(seed)
   {}

   int get_int() 
   {
      return (seed = seed * a + c) & 0x7fffffff;
   }

   double get_double() 
   {
      static const double lo = -1000;
      static const double hi = 1000;
      static const double scale = 1. / (1 << 30);
      return (lo + (hi - lo) * scale * (get_int() & 0x3fffffff));
   }
};

int main()
{
    using std::cin;
    using std::cout;
    int n, t;
    cin >> n >> t;
    if (!t)
    {
        int res = 0;
        int p = 1;
        point A, B, C, D;
        for (int i = 0; i < n; ++i, p *= 239)
        {
            cin >> A.x >> A.y
                >> B.x >> B.y
                >> C.x >> C.y
                >> D.x >> D.y;
            if (segments_intersects(A, B, C, D))
            {
                res += p;
            }
        }
        cout << res << '\n';
        return 0;
    }
    int seed;
    cin >> seed;
    random_input gen(seed);
    int res = 0;
    int p = 1;
    point A, B, C, D;
    for (int i = 0; i < n; ++i, p *= 239)
    {
        A.x = gen.get_double(); A.y = gen.get_double();
        B.x = gen.get_double(); B.y = gen.get_double();
        C.x = gen.get_double(); C.y = gen.get_double();
        D.x = gen.get_double(); D.y = gen.get_double();
        if (segments_intersects(A, B, C, D))
        {
            res += p;
        }
    }
    cout << res << '\n';
    return 0;
}



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
        double x1, y1, x2, y2;
        double x3, y3, x4, y4;
        int res = 0;
        int p = 1;
        for (int i = 0; i < n; ++i, p *= 239)
        {
            cin >> x1 >> y1 >> x2 >> y2 >> x3 >> y3 >> x4 >> y4;
            point A(x1, y1);
            point B(x2, y2);
            point C(x3, y3);
            point D(x4, y4);
            segment AB(A, B);
            segment CD(C, D);
            if (segments_intersects(AB, CD))
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
    double x1, y1, x2, y2;
    double x3, y3, x4, y4;
    int res = 0;
    int p = 1;
    for (int i = 0; i < n; ++i, p *= 239)
    {
        x1 = gen.get_double(); y1 = gen.get_double();
        x2 = gen.get_double(); y2 = gen.get_double();
        x3 = gen.get_double(); y3 = gen.get_double();
        x4 = gen.get_double(); y4 = gen.get_double();
        point A(x1, y1);
        point B(x2, y2);
        point C(x3, y3);
        point D(x4, y4);
        segment AB(A, B);
        segment CD(C, D);
        if (segments_intersects(AB, CD))
        {
            res += p;
        }
    }
    cout << res << '\n';
    return 0;
}



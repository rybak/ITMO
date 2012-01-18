#include <vector>
//#include <cstdio>
#include <iostream>
#include "geometry.h"

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
    
    //scanf("%d%d", &n, &t);
    cin >> n >> t;

    if (!t)
    {
        //double x1, y1, x2, y2;
        //double x3, y3, x4, y4;
        int res = 0;
        int p = 1;
        point A, B, C, D;
        for (int i = 0; i < n; ++i, p *= 239)
        {
            //scanf("%lf%lf%lf%lf%lf%lf%lf%lf", &x1, &y1, &x2, &y2, &x3, &y3, &x4, &y4);
            //cin >> x1 >> y1 >> x2 >> y2 >> x3 >> y3 >> x4 >> y4;
            //point A(x1, y1), B(x2, y2), C(x3, y3), D(x4, y4);
            cin >> A.x >> A.y
                >> B.x >> B.y
                >> C.x >> C.y
                >> D.x >> D.y;
            if (segments_intersects(A, B, C, D))
            {
                res += p;
            }
        }
        //printf("%d\n", res);
        cout << res << '\n';
        return 0;
    }
    int seed;
    //scanf("%d", &seed);
    cin >> seed;
    random_input gen(seed);
    //double x1, y1, x2, y2;
    //double x3, y3, x4, y4;
    int res = 0;
    int p = 1;
    point A, B, C, D;
    for (int i = 0; i < n; ++i, p *= 239)
    {   
        A.x = gen.get_double(); A.y = gen.get_double();
        B.x = gen.get_double(); B.y = gen.get_double();
        C.x = gen.get_double(); C.y = gen.get_double();
        D.x = gen.get_double(); D.y = gen.get_double();
        /*x1 = gen.get_double(); y1 = gen.get_double();
        x2 = gen.get_double(); y2 = gen.get_double();
        x3 = gen.get_double(); y3 = gen.get_double();
        x4 = gen.get_double(); y4 = gen.get_double();*/
        // point A(x1, y1), B(x2, y2), C(x3, y3), D(x4, y4);
        if (segments_intersects(A, B, C, D))
        {
            res += p;
        }
    }
    //printf("%d\n", res);
    cout << res << '\n';
    return 0;
}

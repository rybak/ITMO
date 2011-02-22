#include <fstream>
#include <cmath>
#include <vector>

// polygon square

int main()
{
   std::ifstream input("in.txt");
   std::ofstream output("out.txt");

   struct point
   {
      double x, y;
   };

   std::vector<point> a;
   point p, p1, p2;
   double t1, t2;

   while (input >> t1 >> t2)
   {
      p.x = t1;
      p.y = t2;
      a.push_back(p);
   }
   
   double s = 0;
   for (unsigned i = 0; i < a.size(); i++)
   {
      p1 = i ? a[i-1] : a.back();
      p2 = a[i];
      s += (p1.x - p2.x) * (p1.y + p2.y);
   }
   s /= 2;

   output << fabs(s);

   return 0;
}

#include <fstream>
#include <cassert>

// Ax+By=C

long long gcd (long long a, long long b, long long & x, long long & y)
{
   if ((a + b == a) || (a + b == b) )
   {
      x = 0; y = 1;
      if (b == 0)
         x = 1;
      y = y - x;
      return a + b;
   }
   long long x1, y1;
   long long d = gcd (b%a, a, x1, y1);
   x = y1 - (b / a) * x1;
   y = x1;
   return d;
}

long long gcd (long long a, long long b)
{
   return b ? gcd(b, a % b) : a;
}

int main()
{
   std::ifstream input("in.txt");
   std::ofstream output("out.txt");

   long long a, b, c;

   while (input >> a >> b >> c)
   {
      if ((a == 0) && (b == 0))
      {
         if (c == 0)
            output << "0 0\n";
         else
            output << "<none>\n";
         continue;
      }
      if (c % gcd(a, b) == 0)
      {
         long long x, y;
         long long g = gcd(a, b, x, y);
         long long a1 = x * c / g;
         long long a2 = y * c / g;
         
         output << a1 << ' ' << a2 << '\n';
         assert(a1 * a + a2 * b == c);
         continue;
      }
      else
         output << "<none>\n";
      }

   return 0;
}

#include <fstream>

// Ax+By=C


int gcd (int a, int b, int & x, int & y) {
   if (a == 0) {
      x = 0; y = 1;
      return b;
   }
   int x1, y1;
   int d = gcd (b%a, a, x1, y1);
   x = y1 - (b / a) * x1;
   y = x1;
   return d;
}

int gcd (int a, int b) {
   return b ? gcd(b, a % b) : a;
}

int main()
{
   std::ifstream input("in.txt");
   std::ofstream output("out.txt");

   int a, b, c;

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
      if (a == 0)
      {
         if (c == 0)
            output << "0 0\n";
         else
         {
            if (c % b != 0)
               output << "<none>\n";
            else
               output << "0 " << c / b << '\n';
         }
         continue;
      }
      if (b == 0)
      {
         if (c == 0)
            output << "0 0\n";
         else
         {
            if (c % a != 0)
               output << "<none>\n";
            else
               output << c / a << " 0" <<'\n';
         }
         continue;
      }
      if (c % gcd(a, b) == 0)
      {
         int x, y;
         int g = gcd(a, b, x, y);
         output << x * c / g << ' ' << y * c / g << '\n';
         continue;
      }
      else
         output << "<none>\n";
      }

   return 0;
}

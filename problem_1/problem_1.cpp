#include <fstream>

// spiral

int main()
{
   std::ifstream input("in.txt");
   std::ofstream output("out.txt");

   int n = 0, i = 1, x = 0, y = 0, dx = 1, dy = 0;
   bool c;

   input >> n;
   output << x << ' ' << y << '\n';

   while (i < n)
   {
      i++;
      x = x + dx;
      y = y + dy;
      output << x << ' ' << y << '\n';
      c = false;
      if (!c && (dx > 0))
      {
         dy = dx;
         dx = 0;
         c = true;
      }
      if (!c && (dy > 0))
      {
         dx = -dy - 1;
         dy = 0;
         c = true;
      }
      if (!c && (dx < 0))
      {
         dy = dx;
         dx = 0;
         c = true;
      }
      if (!c && (dy < 0))
      {
         dx = - dy + 1;
         dy = 0;
         c = true;
      }
   }

   return 0;
}

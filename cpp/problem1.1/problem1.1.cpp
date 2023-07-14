#include <fstream>

int main()
{
   std::ifstream input("in.txt");
   std::ofstream output("out.txt");

   int n = 0, i = 1, x = 0, y = 0, dx = 1, dy = 0;
   
   input >> n;

   if (n > 0)
      output << x << ' ' << y << '\n';

   while (i < n)
   {
      i++;
      x = x + dx;
      y = y + dy;
      output << x << ' ' << y << '\n';
      if (dx > 0)
      {
         dy = dx;
         dx = 0;
         continue;
      }
      if (dy > 0)
      {
         dx = -dy - 1;
         dy = 0;
         continue;
      }
      if (dx < 0)
      {
         dy = dx;
         dx = 0;
         continue;
      }
      if (dy < 0)
      {
         dx = - dy + 1;
         dy = 0;
         continue;
      }
   }

   return 0;
}

#include <fstream>
#include <string>
#include <vector>

#include "big_int.h"

using std::string;
using std::ifstream;
using std::ofstream;
using std::pair;

int main()
{
   ifstream input("in.txt");
   ofstream output("out.txt");
   big_int a, b;
   const string error = "<error>\n\n"; 
   while (input >> a >> b)
   {
      if (b == big_int(0))
         output << error;
      else
      {
         std::pair<big_int, big_int> ans = a.divmod(b);
         output << ans.first << '\n' << ans.second << "\n\n";
      }
   }
   return 0;
}

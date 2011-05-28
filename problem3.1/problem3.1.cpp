#include <fstream>
#include "big_int.h"

#include <string>

#include "calculator.h"

using std::ifstream;
using std::ofstream;
using std::string;

const string error = "<error>";

int main()
{
   ifstream input("in.txt");
   ofstream output("out.txt");
   ofstream debug_out("debug.txt");
   while (input)
   {
      string s;
      getline(input, s);
      if (s.length() > 0)
         try
         {
            output << evaluate(s) << "\n";
            //output << p.parse(s, debug_out) << "\n";
         }
         catch (big_int_calc_error)
         {
            output << error << "\n";
         }
   }
   
   /*big_int a, b;
   input >> a >> b;
   output << power(a, b);
   */
   return 0;
}

#include <fstream>
#include <string>
#include <vector>
using std::string;

#include "big_int.h"
// big_int_t

using std::ifstream;
using std::ofstream;

int main()
{
   ifstream input("in.txt");
   ofstream output("out.txt");
   
   big_int_t a, b, c;
   input >> a >> b;
   c = a * b;
   output << c << "\n";
   c = b * a;
   output << c << "\n";
   return 0;
}

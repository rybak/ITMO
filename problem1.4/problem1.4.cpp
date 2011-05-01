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
   
   big_int_t a, b;
   long long c;
   input >> a >> b >> c;
   output << a + b << "\n" << a - b << "\n" << a * b << "\n" << b * a << "\n" << a * c << "\n" << b * c << "\n";
   return 0;
}

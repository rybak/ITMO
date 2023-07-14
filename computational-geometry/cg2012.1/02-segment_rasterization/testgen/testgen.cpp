#include <boost\random.hpp>
#include <boost\lexical_cast.hpp>
#include <fstream>
#include <cmath>
#include <string>
#include <sstream>
#include <ctime>
#include <vector>

const int seed = 477;
const int precision = 30;

struct segment
{
   double x1, y1, x2, y2;
};

std::vector<segment> generateSegments(int n, double range, double delta)
{
   using namespace boost::random;
   mt19937 generator;
   generator.seed(std::time(0));
   uniform_real_distribution<double> dist(-range, range);
   std::vector<segment> res(n);
   for(int i = 0; i < n; i++)
   {
      res[i].x1 = dist(generator) + delta;
      res[i].y1 = dist(generator) + delta;
      res[i].x2 = dist(generator) - delta;
      res[i].y2 = dist(generator) - delta;
   }

   return res;
}

int main() {

   using namespace std;

   int n = 20;
   vector<segment> seg = generateSegments(n, 1000, 0);
   for (int i = 0; i < n; i++)
   {
      string testN = boost::lexical_cast<string>(i);
      testN = string(3 - testN.length(), '0') + testN;
      ofstream fout("correctness_tests/" + testN + ".in");
      fout.precision(precision);
      fout << seg[i].x1 << ' ' << seg[i].y1 << '\n';
      fout << seg[i].x2 << ' ' << seg[i].y2 << '\n';
      fout.close();
   }

   int m = 2;
   seg = generateSegments(m, 100, 10000000);
   for (int i = 0; i < m; i++)
   {
      string testN = boost::lexical_cast<string>(i);
      testN = string(3 - testN.length(), '0') + testN;
      ofstream fout("performance_tests/" + testN + ".in");
      fout.precision(precision);
      fout << seg[i].x1 << ' ' << seg[i].y1 << '\n';
      fout << seg[i].x2 << ' ' << seg[i].y2 << '\n';
      fout.close();
   }
    return 0;
}





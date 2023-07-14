#include <stdio.h>
#include <boost\lexical_cast.hpp>
#include <boost\random.hpp>
#include <fstream>
#include <cmath>
#include <string>
#include <sstream>
#include <ctime>
#include <vector>

const int seed = 477;
const int precision = 30;

struct Point
{
   double x, y;
};

std::vector<Point> generate_points(double range, bool perfomance)
{
   using namespace boost::random;
   mt19937 generator;
   int n;
   generator.seed(std::time(0));
   boost::uniform_int<> ammount(1, 100);
   boost::variate_generator<boost::mt19937, boost::uniform_int<>> ammount_gen(generator, ammount);

   if (perfomance == false)
      n = ammount_gen();
   else
      n = 300000;

   uniform_real_distribution<double> dist(-range, range);
   std::vector<Point> res(n);
   for(int i = 0; i < n; i++)
   {
      res[i].x = dist(generator);
      res[i].y = dist(generator);
   }
   return res;
}


int main()
{
   int n = 20;
   std::vector<Point> point_vector;
   for (int i = 0; i < n; i++)
   {
      std::string testN = boost::lexical_cast<std::string>(i + 1);
      testN = std::string(3 - testN.length(), '0') + testN;
      std::ofstream out("correctness_tests/" + testN + ".in");
      out.precision(precision);
      point_vector = generate_points(1000, false);
      out << point_vector.size() << "\n";
      for (int j = 0; j < point_vector.size(); j++)
         out << point_vector[j].x << ' ' << point_vector[j].y << '\n';
      out.close();
   }

   int m = 2;
   point_vector = generate_points(1000, 1);
   for (int i = 0; i < m; i++)
   {
      std::string testN = boost::lexical_cast<std::string>(i + 1);
      testN = std::string(3 - testN.length(), '0') + testN;
      std::ofstream out("performance_tests/" + testN + ".in");
      out.precision(precision);
      point_vector = generate_points(1000, true);
      out << point_vector.size() << "\n";
      for (int j = 0; j < point_vector.size(); j++)
         out << point_vector[j].x << ' ' << point_vector[j].y << '\n';
      out.close();
   }
   return 0;

}
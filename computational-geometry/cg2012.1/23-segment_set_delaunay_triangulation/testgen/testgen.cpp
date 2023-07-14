#include <boost\random.hpp>
#include <boost\lexical_cast.hpp>
#include <fstream>
#include <cmath>
#include <string>
#include <ctime>
#include <map>

#include <CGAL\Segment_2.h>
#include <CGAL\Point_2.h>
#include <CGAL\Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL\Cartesian.h>
#include <CGAL\Triangulation_2.h>

using namespace std;

typedef CGAL::Exact_predicates_inexact_constructions_kernel Kernel;
typedef CGAL::Point_2<Kernel> Point;
typedef CGAL::Triangulation_2<Kernel> Triangulation;

const int precision = 30;

std::vector<Point> generatePoints(int n, double range)
{
   using namespace boost::random;
   mt19937 generator;
   generator.seed(std::time(0));
   uniform_real_distribution<double> dist(-range, range);
   std::vector<Point> res(n);
   for(int i = 0; i < n; i++)
   {
      res[i] = Point(dist(generator), dist(generator));
   }

   return res;
};

//p - вероятность "взять ребро" 0..100
std::vector<std::pair<int, int>> generateNonIntersectSegments(std::vector<Point>& arr, int p)
{
   using namespace boost::random;
   mt19937 generator;
   generator.seed(std::time(0));
   uniform_int_distribution<int> dist(0, 100);

   std::map<Point, int> pointIds;
   for(int i = 0; i < arr.size(); i++)
   {
      pointIds[arr[i]] = i;
   }

   Triangulation td;
   td.insert(arr.begin(), arr.end());

   std::vector<std::pair<int, int>> res;
   
   for(Triangulation::Finite_edges_iterator i = td.finite_edges_begin(); i != td.finite_edges_end(); i++)
   {
      if(dist(generator) < p)
      {
         int nei = i->second;
         Point p1 = i->first->vertex(i->first->cw(nei))->point();
         Point p2 = i->first->vertex(i->first->ccw(nei))->point();
         res.push_back(std::pair<int, int>(pointIds.find(p1)->second, pointIds.find(p2)->second));
      }
   }

   return res;
};

void makeFile(std::string subfolder, int num, vector<Point> aPoints, vector<pair<int, int>> aSegments)
{
   string testN = boost::lexical_cast<string>(num);
   testN = string(3 - testN.length(), '0') + testN;
   ofstream fout(subfolder + testN + ".in");
   fout.precision(precision);
   fout << aPoints.size() << '\n';
   for(int i = 0; i < aPoints.size(); i++)
   {
      fout << aPoints[i].x() << ' ' << aPoints[i].y() << '\n';
   }
      
   fout << aSegments.size() << '\n';
   for(int i = 0; i < aSegments.size(); i++)
   {
      fout << aSegments[i].first + 1 << ' ' << aSegments[i].second + 1 << '\n';
   }

   fout.close();
};

int main() {

   using namespace std;

   int n = 20;

   for (int i = 0; i < n; i++)
   {
      vector<Point> aPoints = generatePoints(3 * i + 5, 1000);  
      vector<pair<int, int>> aSegments = generateNonIntersectSegments(aPoints, (3 * i) % 20);
      makeFile("correctness_tests/", i, aPoints, aSegments);
   }

   vector<Point> aPoints = generatePoints(10000, 10000);  
   vector<pair<int, int>> aSegments = generateNonIntersectSegments(aPoints, 10);
   makeFile("performance_tests/", 0, aPoints, aSegments);

   aPoints = generatePoints(10000, 10000);  
   aSegments = generateNonIntersectSegments(aPoints, 10);
   makeFile("performance_tests/", 1, aPoints, aSegments);
   
   return 0;
}





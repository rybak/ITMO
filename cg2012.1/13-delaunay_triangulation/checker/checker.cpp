#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Partition_traits_2.h>
#include <CGAL/Polygon_2.h>
#include <CGAL/Triangle_2.h>
#include <CGAL/Segment_2.h>
#include <CGAL/Circle_2.h>
#include <CGAL/squared_distance_2.h>
#include <stdio.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef CGAL::Object Object;
typedef CGAL:: Partition_traits_2<K> Traits;
typedef Traits::Point_2 Point;
typedef Traits::Segment_2 Segment;
typedef CGAL::Circle_2<K> Circle;

class TriangleInCircle
{
public:
   int a, b, c;
   Circle circ;
   TriangleInCircle(int a1, int b1, int c1, Circle circ1): a(a1), b(b1), c(c1), circ(circ1)
   {   }
};
int main(int argc, char* argv[])
{
   std::ifstream in(argv[1]);
   std::ifstream out(argv[2]);
   std::vector<Point> point_vector;
   std::vector<bool> used_points;
   std::vector<TriangleInCircle> circle_vector;
   
   int n = 0;
   in >> n;
   Point tmp;
   for (int i = 0; i < n; i++)
   {
      in >> tmp;
      point_vector.push_back(tmp);
   }

   used_points.resize(point_vector.size());
   int ammountOfTriangles = 0, a = 0, b = 0, c = 0;
   out >> ammountOfTriangles;

   for (int i = 0; i < ammountOfTriangles; i++)
   {
      out >> a >> b >> c;
      if (used_points[a - 1] == false)
         used_points[a - 1] = true;
      
      if (used_points[b - 1] == false)
         used_points[b - 1] = true;
      
      if (used_points[c - 1] == false)
         used_points[c - 1] = true;

      Circle circ(point_vector[a - 1], point_vector[b - 1], point_vector[c - 1]);
      TriangleInCircle tmp(a - 1, b - 1, c - 1, circ);
      circle_vector.push_back(tmp);    
   }

   for (int i = 0; i < used_points.size(); i++)
   {
      if (used_points[i] == false)
         return 1;
   }
   for (int i = 0; i < circle_vector.size(); i++)
   {
      for (int j = 0; j < point_vector.size(); j++)
      {
         if(circle_vector[i].circ.squared_radius() > squared_distance(point_vector[j], circle_vector[i].circ.center())) 
         {
            return 1;
         }
      }
   }
   return 0;
}

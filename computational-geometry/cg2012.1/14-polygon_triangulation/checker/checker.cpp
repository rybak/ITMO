#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Polygon_2.h>
#include <CGAL/Triangle_2.h>
#include <CGAL/Segment_2.h>
#include <stdio.h>
#include <CGAL/Partition_traits_2.h>
#include <CGAL/Boolean_set_operations_2.h>
#include <CGAL/intersections.h>
#include <CGAL/Object.h>
#include <boost/lexical_cast.hpp>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef CGAL::Object Object;
typedef CGAL::Partition_traits_2<K> Traits;
typedef Traits::Point_2 Point;
typedef Traits::Polygon_2 Polygon;
typedef Traits::Segment_2 Segment;
typedef CGAL::Triangle_2<K> Triangle;
typedef std::vector<Point> Vector;

Vector point_vector;
std::string make_str(int a, int b)
{
   return boost::lexical_cast<std::string>(a) + " " + boost::lexical_cast<std::string>(b);
}
void add_edges(std::string key1, std::string key2, std::map<std::string, Segment>& segment_map, int a, int b)
{
   Segment sg(point_vector[a], point_vector[b]);
   std::pair<std::string, Segment> straight_edge (key1, sg);
   std::pair<std::string, Segment> reverse_edge (key2, sg);
   segment_map.insert(straight_edge);
   segment_map.insert(reverse_edge);
}

int main(int argc, char* argv[])
{
   std::map<std::string, Segment> segment_map;
   std::vector<Triangle> triangle_vector;
   int points_ammount_of_outter_hull = 0;
   int ammount_of_holes = 0;
   int current_point_number = 0;
   int total_points_ammount = 0;
   int ammount_of_points_in_hole = 0;

   std::ifstream in(argv[1]);
   std::ifstream out(argv[2]);

//input
   in >> points_ammount_of_outter_hull;
   Point tmp;
   std::string key1;
   std::string key2;
   for (int i = 0; i < points_ammount_of_outter_hull; i++)
   {
      in >> tmp;
      point_vector.push_back(tmp);
      current_point_number = point_vector.size();
      if (current_point_number > 1)
      {
         key1 = make_str(current_point_number, current_point_number - 1);
         key2 = make_str(current_point_number - 1, current_point_number);
         add_edges(key1, key2, segment_map, current_point_number - 1, current_point_number - 2);
      }
   }
   key1 = make_str(current_point_number, 1);
   key2 = make_str(1, current_point_number);
   add_edges(key1, key2, segment_map, 0, current_point_number - 1);

   in >> ammount_of_holes;
   for (int i = 0; i < ammount_of_holes; i++)
   {
      int ammount_of_points_in_hole = 0;
      in >> ammount_of_points_in_hole;
      total_points_ammount = current_point_number;
      for (int j = 0; j < ammount_of_points_in_hole; j++)
      {
         in >> tmp;
         point_vector.push_back(tmp);
         current_point_number++;
         if (current_point_number > ammount_of_points_in_hole + 1)
         { 
            key1 = make_str(current_point_number, current_point_number - 1);
            key2 = make_str(current_point_number - 1, current_point_number);
            add_edges(key1, key2, segment_map, current_point_number - 1, current_point_number - 2);
         }
         else
         {
            if (current_point_number == total_points_ammount + ammount_of_points_in_hole)
            {
               key1 = make_str(total_points_ammount, total_points_ammount + ammount_of_points_in_hole);
               key2 = make_str(total_points_ammount + ammount_of_points_in_hole, total_points_ammount);
               add_edges(key1, key2, segment_map, total_points_ammount - 1, total_points_ammount + ammount_of_points_in_hole - 1);
             }
         }
      }
   }  

////output
   Point point;
   Segment segm;
   int a = 0, b = 0, c = 0, ammount_of_points_res = 0, ammount_of_edges = 0;
   int count = point_vector.size() + 2 * ammount_of_holes - 2;
   out >> ammount_of_points_res;
   if (ammount_of_points_res != count)
      return 1;

   for (int i = 0; i < count; i++)
   {
      out >> a >> b >> c;
      if ((a == b) || (b == c) || (a == c))
         return 1;

      if ((segment_map.find(make_str(a, b)) != segment_map.end()) || (segment_map.find(make_str(b, a)) != segment_map.end())) 
         ammount_of_edges++;
      if ((segment_map.find(make_str(a, c)) != segment_map.end()) || (segment_map.find(make_str(c, a)) != segment_map.end())) 
         ammount_of_edges++;
      if ((segment_map.find(make_str(b, c)) != segment_map.end()) || (segment_map.find(make_str(c, b)) != segment_map.end())) 
         ammount_of_edges++;    
      triangle_vector.push_back(Triangle(point_vector[a - 1], point_vector[b - 1], point_vector[c - 1]));
   }

   if (ammount_of_edges != (segment_map.size()) / 2)
      return 1;

   for (int i = 0; i < count; i++)
   {
      for (int j = 0; j < count; j++)
      {
         if (i != j)
         {
            Object obj = intersection(triangle_vector[i], triangle_vector[j]);
            if ((obj.is_empty()) || (assign(segm, obj)) || (assign(point, obj))) 
            {    }
            else
               return 1;
         }
      }
   }
   return 0;
}
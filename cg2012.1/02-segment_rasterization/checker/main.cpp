#include <fstream>
#include <iostream>
#include <CGAL\Segment_2.h>
#include <CGAL\intersection_2.h>
#include <CGAL\Point_2.h>
#include <CGAL\Exact_predicates_exact_constructions_kernel.h>
#include <CGAL\Cartesian.h>
#include <cmath>
#include <vector>

using namespace std;

typedef CGAL::Exact_predicates_exact_constructions_kernel Kernel;
typedef CGAL::Segment_2<Kernel> Segment;
typedef CGAL::Point_2<Kernel> Point;
typedef CGAL::Point_2<CGAL::Cartesian<double>> Cell;

int main(int argc, char* argv[])
{
   ifstream input(argv[1]);
   double x1, y1, x2, y2;
   input >> x1 >> y1;
   input >> x2 >> y2;

   if(x1 > x2)
   {
      swap(x1, x2);
      swap(y1, y2);
   }

   Segment seg(Point(x1, y1), Point(x2, y2));
   Cell start_cell(floor(x1), floor(y1));
   Cell end_cell(floor(x2), floor(y2));
   Cell cur_cell = start_cell;
   vector<Cell> correct_ans;
   correct_ans.push_back(start_cell);

   while(cur_cell != end_cell)
   {
      double x = cur_cell.x();
      double y = cur_cell.y();
      bool north = CGAL::do_intersect(seg, Segment(Point(x, y + 1), Point(x + 1, y + 1)));
      bool east = CGAL::do_intersect(seg, Segment(Point(x + 1, y), Point(x + 1, y + 1)));
      bool south = CGAL::do_intersect(seg, Segment(Point(x, y), Point(x + 1, y)));

      north = y1 < y2;
      south = y1 > y2;

      if(north)
      {
         y += 1;
      }

      if(east)
      {
         x += 1;
      }

      if(south)
      {
         y -= 1;
      }

      cur_cell = Cell(x, y);
      correct_ans.push_back(cur_cell);
   }

   input.close();
   ifstream output(argv[2]);

   int n;
   vector<Cell> ans;
   output >> n;
   for(int i = 0; i < n; i++)
   {
      int x, y;
      output >> x >> y;
      ans.push_back(Cell(x, y));
   }

   output.close();

   if(correct_ans.size() != ans.size())
   {
      cerr << "WA\n";
      return 1;
   }

   for(int i = 0; i < correct_ans.size(); i++)
   {
      if(ans[i] != correct_ans[i])
      {
         cerr << "WA\n";
         return 1;
      }
   }

   cerr << "AC\n";
   return 0;
}
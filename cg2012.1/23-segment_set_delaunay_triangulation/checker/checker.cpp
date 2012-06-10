#include <fstream>
#include <iostream>
#include <cmath>
#include <vector>

#include <CGAL\Segment_2.h>
#include <CGAL\intersection_2.h>
#include <CGAL\Point_2.h>
#include <CGAL\Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL\Cartesian.h>
#include <CGAL\Triangle_2.h>
#include <CGAL\Triangulation_2.h>
#include <CGAL\predicates_on_points_2.h>

using namespace std;

typedef CGAL::Exact_predicates_inexact_constructions_kernel Kernel;
typedef CGAL::Segment_2<Kernel> Segment;
typedef CGAL::Point_2<Kernel> Point;
typedef CGAL::Triangle_2<Kernel> Triangle;
typedef CGAL::Triangulation_2<Kernel> Triangulation;

bool isSegment(const CGAL::Object& obj)
{
   if(const CGAL::Segment_2<Kernel> *iseg = CGAL::object_cast<CGAL::Segment_2<Kernel>>(&obj))
   {
      return true;
   }
   return false;
};

bool isPoint(const CGAL::Object& obj)
{
   if(const CGAL::Point_2<Kernel> *ipoint = CGAL::object_cast<CGAL::Point_2<Kernel>>(&obj))
   {
      return true;
   }
   return false;
};

int main(int argc, char* argv[])
{
   using namespace CGAL;
   ifstream input(argv[1]);
   int nPoints = 0;
   int nSegments = 0;
   input >> nPoints;
   vector<Point> aPoints(nPoints);

   for(int i = 0; i < nPoints; i++)
   {
      Point p;
      input >> p;
      aPoints[i] = p;
   }

   input >> nSegments;
   vector<Segment> aSegments(nSegments);

   for(int i = 0; i < nSegments; i++)
   {
      int a, b;
      input >> a >> b;
      aSegments[i] = Segment(aPoints[a - 1], aPoints[b - 1]);
   }

   input.close();
   ifstream output(argv[2]);

   int nTriangles = 0;

   output >> nTriangles;

   vector<Triangle> aTriangles(nTriangles);
   vector<bool> bIsConstrainedUsed(nSegments, false); //использованные constrained ребра

   Triangulation td;
   td.insert(aPoints.begin(), aPoints.end());

   if(td.number_of_faces() != nTriangles)
   {
      cerr << "WA\n";
      return 1;
   }

   for(int i = 0; i < nTriangles; i++)
   {
      int a, b, c;
      output >> a >> b >> c;
      aTriangles[i] = Triangle(aPoints[a - 1], aPoints[b - 1], aPoints[c - 1]);

      vector<Segment> constrSegments; // список constrained ребер у считанного треугольника

      for(int j = 0; j < nSegments; j++)  // заполнение constrSegments
      {
         if(aTriangles[i].has_on_boundary(aSegments[j].source()) && 
            aTriangles[i].has_on_boundary(aSegments[j].target()))
         {
            bIsConstrainedUsed[j] = true;
            constrSegments.push_back(aSegments[j]);
         }
      }

      for(int j = 0; j < i; j++) // перебираем предыдущие считанные треугольники и проеряем constrained критерий Делоне
      {
         Triangle t1 = aTriangles[i];
         Triangle t2 = aTriangles[j];
         if(CGAL::do_intersect(t1, t2))
         {
            CGAL:: Object obj = CGAL::intersection(t1, t2);
            if(!isSegment(obj) && !isPoint(obj)) // не должны накладываться
            {
               cerr << "WA\n";
               return 1;
            }
            
            if(isSegment(obj))
            {
               Segment s;
               CGAL::assign(s, CGAL::intersection(t1, t2));
               bool isSConstrained = false; // является ли общее ребро constrained
               for(int k = 0; k < constrSegments.size(); k++)
               {
                  if(constrSegments[k].min() == s.min() && constrSegments[k].max() == s.max())
                  {
                     isSConstrained = true;
                  }
               }

               for(int k = 0; k < 3; k++) // собс-но критерий
               {
                  if(CGAL::side_of_bounded_circle(t1[0], t1[1], t1[2], t2[k]) == CGAL::ON_BOUNDED_SIDE && !isSConstrained)
                  {
                     cerr << "WA\n";
                     return 1;
                  }
               }

               for(int k = 0; k < 3; k++)
               {
                  if(CGAL::side_of_bounded_circle(t2[0], t2[1], t2[2], t1[k]) == CGAL::ON_BOUNDED_SIDE && !isSConstrained)
                  {
                     cerr << "WA\n";
                     return 1;
                  }
               }
            }
         }
      }
   }

   for(int i = 0; i < nSegments; i++) // есть ли неиспользованные constrained ребра
   {
      if(!bIsConstrainedUsed[i])
      {
         cerr << "WA\n";
         return 1;
      }
   }

   output.close();

   cerr << "AC\n";
   return 0;
}
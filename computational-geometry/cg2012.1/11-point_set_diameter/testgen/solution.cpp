#include <iostream>
#include <fstream>
#include <vector>
#include <iterator>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/convex_hull_2.h>
#include <CGAL/enum.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel Kernel;
typedef Kernel::Point_2 Point_2;

int main(int argc, char* argv[]) {
  std::ifstream input(argv[1]);
  int n;
  input >> n;
  std::vector<Point_2> points(n);
  for (int i = 0; i < n; i++) {
    double x, y;
    input >> x >> y;
    points[i] = Point_2(x, y);
  }
  input.close();

  int m1 = 0, m2 = 1;

  Kernel::Compare_distance_2 cmp2;
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      if (cmp2(points[m1], points[m2], points[i], points[j]) == CGAL::SMALLER) {
	m1 = i;
	m2 = j;
      }
    }
  }

  std::cout << points[m1] << ' ' << points[m2];
  return 0;
}

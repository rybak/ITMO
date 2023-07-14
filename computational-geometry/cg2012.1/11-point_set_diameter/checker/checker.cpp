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
  if (argc != 3) {
    std::cerr << "Usage: pointsetdiam-checker infile outfile" << std::endl;
    return -1;
  }

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

  std::ifstream output(argv[2]);
  double x, y;
  output >> x >> y;
  Point_2 p1(x, y);
  output >> x >> y;
  Point_2 p2(x, y);

  int p1i = -1, p2i = -1;
  for (int i = 0; i < n; i++) {
    if (points[i] == p1) {
      p1i = i;
    }
  }
  if (p1i == -1)
    return -1;

  for (int i = 0; i < n; i++) {
    if (points[i] == p2 && i != p1i) {
      p2i = i;
    }
  }
  if (p2i == -1)
    return -1;

  Kernel::Compare_distance_2 cmp2;
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      if (cmp2(p1, p2, points[i], points[j]) == CGAL::SMALLER) {
	return -1;
      }
    }
  }

  return 0;
}

#include <iostream>
#include <fstream>
#include <vector>
#include <set>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/distance_predicates_2.h>
#include <CGAL/enum.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel::Point_2 Point;
typedef std::vector<Point> Vector;
typedef std::set<Point> Set;

Set closest_point(const Vector& points, const Point query) {
	Set result;
	Point result_point;
	if (!points.empty()) {
		result.insert(points[0]);
		result_point = points[0];
	}
	 
	size_t size = points.size();
	for (size_t i = 0; i < size; i++) {
		Point point = points[i];
	
		if (CGAL::compare_distance_to_point(query, point, result_point) == CGAL::EQUAL) {
			result.insert(point);
		} else if (CGAL::compare_distance_to_point(query, point, result_point) == CGAL::SMALLER) {
			result.clear();
			result.insert(point);
			result_point = point;
		}
	}
	
	return result;
}

int main(int argc, char* argv[]) {
	std::ifstream in(argv[1]);
	std::ifstream out(argv[2]);
	
	size_t n, m;
	double x, y;
	
	in >> n;	
	Vector s(n);
	
	for (size_t i = 0; i < n; i++) {
		in >> x >> y;
		s[i] = Point(x, y);
	}
	
	in >> m;
	bool is_correct = true;
		
	for (int i = 0; i < m; i++) {		
		in >> x >> y;
		Set correct_answer = closest_point(s, Point(x, y));
		
		out >> x >> y;
		if (correct_answer.find(Point(x, y)) == correct_answer.end()) {
			is_correct = false;
			break;
		}
	}
	
	in.close();
	out.close();
	
	if (is_correct) {
		std::cerr << "AC\n";
		return 0;
	} else {
		std::cerr << "WA\n";
		return 1;
	}
}

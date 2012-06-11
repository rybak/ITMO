#include <fstream>
#include <iostream>
#include <cmath>
#include <vector>
#include <set>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef K::Point_2 Point;
typedef K::Segment_2 Segment;
typedef std::vector<Point> Vector;
typedef std::set<Point> Set;

double floor_x(const Point p) {
	return std::floor(CGAL::to_double(p.x()));
}

double floor_y(const Point p) {
	return std::floor(CGAL::to_double(p.y()));
}

Point floor_point(const Point p) {
	return Point(floor_x(p), floor_y(p));
}

Vector rasterization_points(const Vector vertices) {
	size_t n = vertices.size();
	Set result_set;
	
	for (size_t i = 0; i < n; i++) {
		Point a = vertices[i % n];
		Point b = vertices[(i + 1) % n];
		
		if (a > b) {
			std::swap(a, b);
		}
		
		Segment segment = Segment(a, b);
		bool is_rising = a.y() <= b.y();
		
		Point begin = floor_point(a);
		Point end = floor_point(b);
		Point current = begin;
		
		double x = floor_x(begin);
		double y = floor_y(begin);
		
		while (current != end) {
			result_set.insert(current);
			
			if (is_rising) {
				if (CGAL::do_intersect(segment, Segment(Point(x + 1, y), Point(x + 1, y + 1)))) {
					x += 1;
				}				
				if (CGAL::do_intersect(segment, Segment(Point(x, y + 1), Point(x + 1, y + 1)))) {
					y += 1;
				}
			} else {
				if (CGAL::do_intersect(segment, Segment(Point(x + 1, y), Point(x + 1, y + 1)))) {
					x += 1;
				} else {
					y -= 1;
				}
			}
			
			current = Point(x, y);
		}
		
		result_set.insert(end);
	}
	
	Vector result;
	for (Set::iterator it = result_set.begin() ; it != result_set.end(); it++) {
		result.push_back(*it);
	}
	std::sort(result.begin(), result.end());
	
	return result;
}

int main(int argc, char* argv[]) {
	std::ifstream in(argv[1]);
	size_t n = 3;
	
	Vector vertices(n);
	for (size_t i = 0; i < n; i++) {
		in >> vertices[i];
	}
	
	in.close();	
	Vector correct_answer = rasterization_points(vertices);
	
	std::ifstream out(argv[2]);
	size_t m;
	out >> m;
	
	Vector answer;
	Point p;
	while (out >> p) {
		answer.push_back(p);
	}
	
	out.close();	
	bool is_correct = correct_answer.size() == answer.size() && answer.size() == m;
	
	size_t i = 0;
	while (is_correct && i < m) {
		if (answer[i] == correct_answer[i]) {
			i++;
		} else {
			is_correct = false;
		}
	}
	
	if (is_correct) {
		std::cerr << "AC\n";
		return 0;
	} else {
		std::cerr << "WA\n";
		return 1;
	}
}

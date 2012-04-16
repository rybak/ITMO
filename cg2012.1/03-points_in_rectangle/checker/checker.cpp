#include <iostream>
#include <fstream>
#include <vector>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef K::Point_2 Point;
typedef K::Iso_rectangle_2 Rectangle;
typedef std::vector<Point> Vector;

Vector points_in_rectangle(const Vector& points, Rectangle rectangle) {
	Vector result;
	
	for (size_t i = 0; i < points.size(); i++) {
		Point point = points[i];
	
		if (!rectangle.has_on_unbounded_side(point)) {
			result.push_back(point);
		}
	}
	
	return result;
}

int main(int argc, char* argv[]) {
	std::ifstream in("test.in");
	std::ifstream out("test.out");
	
	size_t n;	
	in >> n;	
	Vector s(n);
	
	for (size_t i = 0; i < n; i++) {
		double x, y;
		in >> x >> y;
		s[i] = Point(x, y);
	}
	
	size_t m;
	in >> m;
	bool is_correct = true;
	
	for (size_t i = 0; i < m; i++) {
		double a_x, a_y, b_x, b_y;		
		in >> a_x >> a_y >> b_x >> b_y;
		
		Vector correct_answer = points_in_rectangle(s, Rectangle(Point(a_x, a_y), Point(b_x, b_y)));
		std::sort(correct_answer.begin(), correct_answer.end());
		
		size_t count;
		out >> count;
		
		Vector answer(count);
		for (size_t i = 0; i < count; i++) {
			out >> answer[i];
		}
		std::sort(answer.begin(), answer.end());
		
		if (correct_answer != answer) {
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

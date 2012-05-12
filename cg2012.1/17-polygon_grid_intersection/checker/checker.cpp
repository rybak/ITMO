#include <iostream>
#include <fstream>
#include <vector>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef K::Point_2 Point;
typedef K::Direction_2 Direction;
typedef K::Line_2 Line;
typedef std::vector<Point> Vector;


std::pair<Point, Point> limits(const Direction direction, const Point begin, const Point end) {
	Point result_begin, result_end;

	if (direction.dx() > 0) {
		if (direction.dy() > 0) {
			result_begin = Point(std::ceil(begin.x()), std::ceil(begin.y()));
			result_end = Point(std::ceil(end.x()), std::ceil(end.y()));
		} else if (direction.dy() < 0) {
			result_begin = Point(std::ceil(begin.x()), std::floor(begin.y()));
			result_end = Point(std::ceil(end.x()), std::floor(end.y()));
		} else {
			result_begin = Point(std::ceil(begin.x()), begin.y());
			result_end = Point(std::ceil(end.x()), end.y());
		}
	} else if (direction.dx() < 0) {
		if (direction.dy() > 0) {
			result_begin = Point(std::floor(begin.x()), std::ceil(begin.y()));
			result_end = Point(std::floor(end.x()), std::ceil(end.y()));
		} else if (direction.dy() < 0) {
			result_begin = Point(std::floor(begin.x()), std::floor(begin.y()));
			result_end = Point(std::floor(end.x()), std::floor(end.y()));
		} else {
			result_begin = Point(std::floor(begin.x()), begin.y());
			result_end = Point(std::floor(end.x()), end.y());
		}
	} else {
		if (direction.dy() > 0) {
			result_begin = Point(begin.x(), std::ceil(begin.y()));
			result_end = Point(end.x(), std::ceil(end.y()));
		} else {
			result_begin = Point(begin.x(), std::floor(begin.y()));
			result_end = Point(end.x(), std::floor(end.y()));
		}
	}

	return std::make_pair(result_begin, result_end);
}


void merge(Vector& result, const Vector x_points, const Vector y_points, const Direction direction) {
	size_t i = 0, j = 0;

	while (i < x_points.size() && j < y_points.size()) {
		if ((direction.dx() >= 0 && x_points[i].x() <= y_points[j].x()) || (direction.dx() < 0 && x_points[i].x() >= y_points[j].x())) {
			result.push_back(x_points[i]);

			if (x_points[i].x() == y_points[j].x()) {
				j++;
			}
			i++;
		} else {
			result.push_back(y_points[j]);
			j++;
		}
	}

	while (i < x_points.size()) {
		result.push_back(x_points[i]);
		i++;
	}
	while (j < y_points.size()) {
		result.push_back(y_points[j]);
		j++;
	}
}

Vector grid_intersection(const Vector vertices) {
	Vector result;

	for (size_t i = 0; i < vertices.size(); i++) {
		Point begin = vertices[i % vertices.size()], end = vertices[(i + 1) % vertices.size()];
		Vector x_points, y_points;

		Line line = Line(begin, end);
		Direction direction = line.direction();
		std::pair<Point, Point> pair = limits(direction, begin, end);

		if (direction.dx() > 0) {
			for (double x = pair.first.x(); x < pair.second.x(); x += 1) {
				x_points.push_back(Point(x, line.y_at_x(x)));
			}
		} else if (direction.dx() < 0) {
			for (double x = pair.first.x(); x > pair.second.x(); x -= 1) {
				x_points.push_back(Point(x, line.y_at_x(x)));
			}
		}

		if (direction.dy() > 0) {
			for (double y = pair.first.y(); y < pair.second.y(); y += 1) {
				y_points.push_back(Point(line.x_at_y(y), y));
			}
		} else if (direction.dy() < 0) {
			for (double y = pair.first.y(); y > pair.second.y(); y -= 1) {
				y_points.push_back(Point(line.x_at_y(y), y));
			}
		}

		merge(result, x_points, y_points, direction);
	}

	return result;
}

int main(int argc, char* argv[]) {
	std::ifstream in(argv[1]);
	std::ifstream out(argv[2]);

	size_t n;
	in >> n;
	Vector polygon(n);

	for (size_t i = 0; i < n; i++) {
		double x, y;
		in >> x >> y;
		polygon[i] = Point(x, y);
	}
	Vector correct_answer = grid_intersection(polygon);

	size_t m;
	out >> m;
	bool is_correct = (m == correct_answer.size() ? true : false);

	size_t i = 0;
	while (is_correct && i < m) {
		double x, y;
		out >> x >> y;

		if (Point(x, y) == correct_answer[i]) {
			i++;
		} else {
			is_correct = false;
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

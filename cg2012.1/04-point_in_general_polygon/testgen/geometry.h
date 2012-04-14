#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <vector>

struct Point
{
	double x, y;

	Point()
		: x(0.0), y(0.0)
	{}

	Point(double x, double y)
		: x(x), y(y)
	{}
};

typedef std::vector<Point> Polygon;

#endif
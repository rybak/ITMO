#ifndef GEOMETRY_H
#define GEOMETRY_H

/*
#include <fstream>
std::ofstream debug("geometry_debug.txt");
*/

struct point
{
    double x, y;
    point(double x, double y)
        : x(x), y(y)
    {}
    point()
        : x(0), y(0)
    {}
};

struct segment
{
    point a, b;
    segment(point const &a, point const &b)
        : a(a), b(b)
    {}
};

bool segments_intersects(point const &, point const &, point const &, point const &);

#endif
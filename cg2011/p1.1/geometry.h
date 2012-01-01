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
    segment(const point &a, const point &b)
        : a(a), b(b)
    {}
};

int left_turn(point, point, point);
bool intersects(segment, segment);

#endif
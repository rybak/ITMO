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

bool segments_intersects(const point &, const point &, const point &, const point &);

#endif
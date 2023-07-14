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

bool operator != (const point &, const point &);
int left_turn(const point &, const point &, const point &);
int quickhull_predicate(const point &, const point &, const point &, const point &);

#endif
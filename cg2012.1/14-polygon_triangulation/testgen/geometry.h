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
    
    friend std::ostream& operator<< (std::ostream&, const Point&);
    friend std::istream& operator>> (std::istream&, Point&);
    
};

Point operator* (double, const Point&);
Point operator+ (const Point&, const Point&);
#endif
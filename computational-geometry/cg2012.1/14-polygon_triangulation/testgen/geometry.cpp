#include <iostream>
#include "geometry.h"


std::ostream& operator<< (std::ostream &out, const Point &p)
{
    out << p.x << ' ' << p.y;
    return out;
}

std::istream& operator>> (std::istream &in, Point &p)
{
    in >> p.x >> p.y;
    return in;
}

Point operator* (double a, const Point &p)
{
    return Point(a * p.x, a * p.y);
}

Point operator+ (const Point &a, const Point &b)
{
    return Point(a.x + b.x, a.y + b.y);
}
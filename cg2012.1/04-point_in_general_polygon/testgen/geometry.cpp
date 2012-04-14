#include <iostream>
#include "geometry.h"


std::ostream& operator<< (std::ostream &out, const Point &p)
{
	out << p.x << ' ' << p.y;
	return out;
}
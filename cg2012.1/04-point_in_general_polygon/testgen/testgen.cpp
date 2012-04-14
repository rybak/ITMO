#include <iostream>
#include <fstream>
#include <vector>

#include "geometry.h"

struct test_case
{
	point q;
	Polygon p;
	std::vector<Polygon> holes;
	test_case(const Point q, const Polygon &p, const std::vector<Polygon> &holes)
		: q(q), p(p), holes(holes)
	{}
};


int main()
{
	
	return 0;
}
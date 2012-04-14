#ifndef TEST_CASE_H
#define TEST_CASE_H
#include <vector>
#include <iostream>

#include "geometry.h"

using std::vector;

struct test_case
{
	Point q;
	Polygon p;
	vector<Polygon> holes;
	test_case(const Point q, const Polygon &p, const vector<Polygon> &holes)
		: q(q), p(p), holes(holes)
	{}
	friend std::ostream& operator<< (std::ostream&, const test_case&);
};

vector<test_case> generate_correctness_tests();

#endif
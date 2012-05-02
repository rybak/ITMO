#ifndef TEST_CASE_H
#define TEST_CASE_H
#include <vector>
#include <iostream>

#include "geometry.h"

using std::vector;

struct test_case
{
	Point q;
	vector<Point> p;
	vector<vector<Point> > holes;
	test_case(const Point q, const vector<Point> &p, const vector<vector<Point> > &holes)
		: q(q), p(p), holes(holes)
	{}
	test_case()	
		: p(0), holes(0)
	{}
	friend std::ostream& operator<< (std::ostream&, const test_case&);
};

vector<test_case> generate_correctness_tests();

#endif
#ifndef TEST_CASE_H
#define TEST_CASE_H
#include <vector>
#include <iostream>

#include "geometry.h"

using std::vector;

struct test_case
{
    vector<Point> p;
    vector<vector<Point> > holes;
    
    test_case(const vector<Point> &p, const vector<vector<Point> > &holes)
        : p(p), holes(holes)
    {}
    
    test_case()    
        : p(0), holes(0)
    {}
    
    friend std::ostream& operator<< (std::ostream&, const test_case&);
};

#endif
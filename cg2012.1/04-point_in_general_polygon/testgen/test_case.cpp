#include <cstddef>
#include "test_case.h"

namespace
{
    std::ostream& operator<< (std::ostream& out, const vector<Point> &points)
    {
        size_t n = points.size();
        out << n << '\n';
        for (size_t i = 0; i < n; ++i)
        {
            out << points[i] << '\n';
        }
        return out;
    }
}

std::ostream& operator<< (std::ostream& out, const test_case& test)
{
    out << test.p;
    size_t n = test.holes.size();
    out << n << '\n';
    for (size_t i = 0; i < n; ++i)
    {
        out << test.holes[i];
    }
    
    out << test.q;

    return out;
}
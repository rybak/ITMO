#include <algorithm>
#include <limits>
#include <cmath>
#include "geometry.h"
#include "adaptive.h"

namespace
{
    bool intersect_axis_rectangles(const point &A, const point &B, const point &C, const point &D)
    {
        return !( (C.x > B.x) || (D.x < A.x) || (C.y > B.y) || (D.y < A.y) );
    }
    bool check_bounding_boxes(const point &A, const point &B, const point &C, const point &D)
    {  
        using std::min;
        using std::max;
        return intersect_axis_rectangles
        (
            point(min(A.x, B.x), min(A.y, B.y)),
            point(max(A.x, B.x), max(A.y, B.y)),
            point(min(C.x, D.x), min(C.y, D.y)),
            point(max(C.x, D.x), max(C.y, D.y))
        );
    }
}

int epsilon_left_turn(const point &a, const point &b, const point &c)
{
    double res = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
    static double eps1 = 4 * std::numeric_limits<double>::epsilon();
    double t = std::abs((b.x - a.x) * (c.y - a.y)) + std::abs((b.y - a.y) * (c.x - a.x));
    double eps = eps1 * t;
    if (res > eps)
    {
        return 1;
    }
    if (-res > eps)
    {
        return -1;
    }
    return 0;
}

int left_turn(const point &a, const point &b, const point &c)
{
    int res;
    res = epsilon_left_turn(a, b, c);
    if (res != 0)
    {
        return res;
    }
    res = adaptive_left_turn(a, b, c);
    return res;
}


bool segments_intersects(const point &A, const point &B, const point &C, const point &D)
{
    if (!check_bounding_boxes(A, B, C, D))
    {
        return false;
    }
    if (1 == left_turn(A, B, C) * left_turn(A, B, D))
    {
        return false;
    }
    if (1 ==  left_turn(C, D, A) * left_turn(C, D, B))
    {
        return false;
    }
    return true;
}
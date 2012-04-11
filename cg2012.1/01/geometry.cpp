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

bool operator != (const point &a, const point &b)
{
    return !((a.x == b.x) && (a.y == b.y));
}

int epsilon_left_turn(const point &a, const point &b, const point &c)
{
    double res = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
    static double const eps1 = 4 * std::numeric_limits<double>::epsilon();
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

int epsilon_quickhull_predicate(const point &a, const point &b, const point &c1, const point &c2)
{
    double res = (b.x - a.x) * (c1.y - c2.y) - (b.y - a.y) * (c1.x - c2.x);
    static double eps1 = 4 * std::numeric_limits<double>::epsilon();
    double t = std::abs((b.x - a.x) * (c1.y - c2.y)) + std::abs((b.y - a.y) * (c1.x - c2.x));
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

int quickhull_predicate(const point &a, const point &b, const point &c1, const point &c2)
{
    int res = epsilon_quickhull_predicate(a, b, c1, c2);
    if (res != 0)
    {
        return res;
    }
    res = adaptive_quickhull_predicate(a, b, c1, c2);
    if (res == 0)
    {
        if (left_turn(a, c2, c1) >= 0)
        {
            return 1;
        }
    }
    return res;
}

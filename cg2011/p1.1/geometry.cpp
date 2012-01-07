#include <algorithm>
#include "geometry.h"
#include "interval.h"
#include "adaptive.h"

namespace
{
    bool intersect_axis_rectangles(point const &A, point const &B, point const &C, point const &D)
    {
        return !( (C.x > B.x) || (D.x < A.x) || (C.y > B.y) || (D.y < A.y) );
    }
    bool check_bounding_boxes(point const &A, point const &B, point const &C, point const &D)
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
int left_turn(point const &a, point const &b, point const &c)
{
    int res;
    res = interval_left_turn(a, b, c);
    if (res == 0)
    {
        res = adaptive_left_turn(a, b, c);
    }
    return res;
}

bool segments_intersects(segment const &AB, segment const &CD)
{
    point A(AB.a);
    point B(AB.b);
    point C(CD.a);
    point D(CD.b);
    if (!check_bounding_boxes(A, B, C, D))
    {
        return false;
    }

    int abc = left_turn(A, B, C);
    int abd = left_turn(A, B, D);
    int ab =  abc * abd;
        
    int cda = left_turn(C, D, A);
    int cdb = left_turn(C, D, B);
    int cd =  cda * cdb;
    
    if ((1 == ab) || (1 == cd))
        return false;
    return true;
}

bool segments_intersects(point const &A, point const &B, point const &C, point const &D)
{
    if (!check_bounding_boxes(A, B, C, D))
    {
        return false;
    }

    int abc = left_turn(A, B, C);
    int abd = left_turn(A, B, D);
    int ab =  abc * abd;
        
    int cda = left_turn(C, D, A);
    int cdb = left_turn(C, D, B);
    int cd =  cda * cdb;
    
    if ((1 == ab) || (1 == cd))
        return false;
    return true;
}
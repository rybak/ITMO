#include "boost/numeric/interval.hpp"
//#include 
#include "geometry.h"
#include "interval.h"

namespace
{
    typedef boost::numeric::interval<double> interval;
    const interval interval_zero = interval(0.0);    

    struct interval_point
    {
        interval x, y;

        interval_point(interval const &x, interval const &y)
            : x(x), y(y)
        {}
    };
    typedef interval_point i_point;

}

int interval_left_turn(point const &a, point const &b, point const &c)
{
    using boost::numeric::interval_lib::cergt;
    //typedef interval i;
    interval iax(a.x), ibx(b.x), icx(c.x), iay(a.y), iby(b.y), icy(c.y);
    interval t = (ibx - iax) * (icy - iay) - (iby - iay) * (icx - iax);
    if (t.lower() > 0)
    {
        return 1;
    }
    if (t.upper() < 0)
    {
        return -1;
    }
    return 0;
}

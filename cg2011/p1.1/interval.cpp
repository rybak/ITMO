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

int interval_left_turn(const point &a, const point &b, const point &c)
{
    using boost::numeric::interval_lib::cergt;

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

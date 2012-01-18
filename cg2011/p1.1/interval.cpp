#include "boost/numeric/interval.hpp"
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
    typedef interval i;
    i iax(a.x), ibx(b.x), icx(c.x), iay(a.y), iby(b.y), icy(c.y);
    i t = (ibx - iax) * (icy - iay) - (iby - iay) * (icx - iax);

    /*if (cergt(t, interval_zero))
        return 1;
    if (cergt(interval_zero, t))
        return -1;*/
    if (cergt(t, 0.0))
        return 1;
    if (cergt(0.0, t))
        return -1;
    return 0;
}

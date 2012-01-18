#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <vector>
#include <limits>
#include "geometry.h"
#include "adaptive.h"

using std::pair;
using std::make_pair;
using std::vector;

namespace
{
    double sum(double a, double b, double &roundoff)
    {
        double x = a + b;
        double bv = x - a;
        double av = x - bv;
        double br = b - bv;
        double ar = a - av;
        roundoff = ar + br;
        return x;
    }

    void split(double a, size_t s, double& hi, double& lo)
    {
        double c = ((1LL << s) + 1LL) * a;
        double ab = c - a ;
        hi = c - ab ;
        lo = a - hi ;
    }

    double mul(double a, double b, double& roundoff)
    {
        double x = a * b;
        static size_t s = std::numeric_limits<double>::digits / 2
            + std::numeric_limits<double>::digits % 2;
        
        double a_hi, a_lo, b_hi, b_lo;
        split(a, s, a_hi, a_lo);
        split(b, s, b_hi, b_lo);
        
        double e1 = x - (a_hi * b_hi);
        double e2 = e1 - (a_lo * b_hi);
        double e3 = e2 - (b_lo * a_hi);
        
        roundoff = (a_lo * b_lo) - e3;
        return x;
    }

    template <size_t N>
    struct grow_expansion_f
    {
        static void calc(double const *e, double b, double *r)
        {
            b = sum(*e, b, *r);
            grow_expansion_f<N - 1>::calc(e + 1, b, r + 1);
        }
    };

    template <>
    struct grow_expansion_f<0>
    {
        static void calc(double const *e, double b, double *r)
        {
            *r = b;
        }
    };

    template <size_t N1, size_t N2>
    struct expand_sum_f
    {
        static void calc(double const *e, double const *f, double *r)
        {
            grow_expansion_f<N1>::calc(e, *f, r);
            expand_sum_f<N1, N2 - 1>::calc(r + 1, f + 1, r + 1);
        }
    };

    template <size_t N1>
    struct expand_sum_f<N1, 0>
    {
        static void calc(double const *e, double const *f, double *r) { }
    };

};

int adaptive_left_turn(point const &a, point const &b, point const &c)
{       
    double sa[12];
    sa[1] = mul(b.x, c.y, sa[0]);
    sa[3] = mul(-b.x, a.y, sa[2]);
    sa[5] = mul(-a.x, c.y, sa[4]);
    sa[7] = mul(-b.y, c.x, sa[6]);
    sa[9] = mul(b.y, a.x, sa[8]);
    sa[11] = mul(a.y, c.x, sa[10]);
    
    double sb[12];
    expand_sum_f<2, 2>::calc(sa + 0, sa + 2, sb);
    expand_sum_f<2, 2>::calc(sa + 4, sa + 6, sb + 4);
    expand_sum_f<2, 2>::calc(sa + 8, sa + 10, sb + 8);

    double sc[8];
    expand_sum_f<4, 4>::calc(sb, sb + 4, sc);

    double sd[12];
    expand_sum_f<8, 4>::calc(sb, sb + 8, sd);
    
    for (int i = 11; i >= 0; i--)
    {
        if (sd[i] > 0)
            return 1;
        else if (sd[i] < 0)
            return -1;
    }
    
    return 0;
}
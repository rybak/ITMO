#include <cmath>
#include <limits>
#include "geometry.h"
#include "adaptive.h"

namespace
{
    double sum(double a, double b, double &r)
    {
        double res = a + b;
        double bv = res - a;
        double av = res - bv;
        double br = b - bv;
        double ar = a - av;
        r = ar + br;
        return res;
    }
    
    void split(double a, double &ah, double &al)
    {
        static int s = std::numeric_limits<double>::digits - std::numeric_limits<double>::digits / 2;
        double c = ((1LL << s) + 1LL) * a;
        double ab = c - a;
        ah = c - ab;
        al = a - ah;
    }

    double mul(double a, double b, double &y)
    {
        double x = a * b;
        double ah, al, bh, bl;
        split(a, ah, al);
        split(b, bh, bl);
        double e1 = x - (ah * bh);
        double e2 = e1 - (al * bh);
        double e3 = e2 - (bl * ah);
        y = (al * bl) - e3;
        return x;
    }

    
    template <int N>
    void grow_expansion(double *e, double b, double *h)
    {
        double q = b;
        for(int i = 0; i < N; ++i)
        {
            q = sum(e[i], q, h[i]);
        }
        h[N] = q;
    }

    template <int N1, int N2>
    void expansion_sum(double *e, double *f)
    {
        for(int i = 0; i < N2; ++i)
        {
            grow_expansion<N1>(e + i, f[i], e + i);
        }
    }
};

int adaptive_left_turn(const point &a, const point &b, const point &c)
{
    double p[12];
    
    p[0] = mul(b.x, c.y, p[1]);
    p[2] = mul(-b.x, a.y, p[3]);
    p[4] = mul(-a.x, c.y, p[5]);
    p[6] = mul(-b.y, c.x, p[7]);
    p[8] = mul(b.y, a.x, p[9]);
    p[10] = mul(a.y, c.x, p[11]);

    expansion_sum<2, 2>(p, p + 2);
    expansion_sum<2, 2>(p + 4, p + 6);
    expansion_sum<2, 2>(p + 8, p + 10);
    expansion_sum<4, 4>(p, p + 4);
    expansion_sum<8, 4>(p, p + 8);
    
    for (int i = 11; i >= 0; --i)
    {
        if (p[i] > 0)
        {
            return 1;
        }
        if (p[i] < 0)
        {
            return -1;
        }
    }
    return 0;
}
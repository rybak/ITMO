#include <cmath>
#include <limits>
#include "geometry.h"
#include "adaptive.h"

namespace
{
    double sum(double a, double b, double& r)
    {
        double res = a + b;
        double b_v = res - a;
        double a_v = res - b_v;
        double b_r = b - b_v;
        double a_r = a - a_v;
        r = a_r + b_r;
        return res;
    }
    
    void split(double a, double& ah, double& al)
    {
        static int s = std::numeric_limits<double>::digits - std::numeric_limits<double>::digits / 2;
        double c = ((1LL << s) + 1LL) * a;
        double ab = c - a;
        ah = c - ab;
        al = a - ah;
    }

    double mul(double a, double b, double& roundoff)
    {
        double res = a * b;
        double ah, al, bh, bl;
        split(a, ah, al);
        split(b, bh, bl);
        double e1 = res - (ah * bh);
        double e2 = e1 - (al * bh);
        double e3 = e2 - (bl * ah);
        roundoff = (al * bl) - e3;
        return res;
    }

    template <int N>
    int get_sign(double * e)
    {
        for (int i = N - 1; i >= 0; i--)
        {
            if (e[i] > 0)
            {
                return 1;
            }
            if (e[i] < 0)
            {
                return -1;
            }
        }
        return 0;
    }
    
    template <int N>
    void grow_expansion(double* e, double b, double* h)
    {
        double q = b;
        for(int i = 0; i < N; i++)
        {
            q = sum(e[i], q, h[i]);
        }
        h[N] = q;
    }

    template <int N1, int N2>
    void expansion_sum(double* e, double* f)
    {
        for(int i = 0; i < N2; i++)
        {
            grow_expansion<N1>(e + i, f[i], e + i);
        }
    }
};

int adaptive_left_turn(point const &a, point const &b, point const &c)
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
    
    return get_sign<12>(p);
}
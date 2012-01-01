#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <vector>
#include <climits>
#include <limits>
#include "geometry.h"
#include "adaptive.h"

using std::pair;
using std::make_pair;
using std::vector;

namespace
{
    /***************************************************
        Adaptive Precision Floating-Point Arithmetic
        and Fast Robust Geometric Predicates
        Jonathan Richard Shewchuk
    ****************************************************/

    pair<double, double> sum(double a, double b)
    {
        double x = a + b;
        double bv = x - a;
        double av = x - bv;
        double ar = a - av;
        double br = b - bv;
        double y = ar + br;
        return make_pair<double, double>(x, y);
    }

    
    vector<double> grow_expansion(const vector<double> &e, double b)
    {
        int m = e.size();
        vector<double> q(m + 1);
        vector<double> h(m + 1);
        q[0] = b;
        pair<double, double> tmp;
        for (int i = 0; i < m; ++i)
        {
            tmp = sum(q[i], e[i]);
            q[i + 1] = tmp.first;
            h[i] = tmp.second;
        }
        h[m] = q[m];
        return h;
    }

    vector<double> grow_expansion(const vector<double> &e, double b, int start, int finish)
    {
        int m = finish - start + 1;
        vector<double> q(m + 1);
        vector<double> h(m + 1);
        q[0] = b;
        pair<double, double> tmp;
        for (int i = 0; (i + start) <= finish; ++i)
        {
            tmp = sum(q[i], e[i + start]);
            q[i + 1] = tmp.first;
            h[i] = tmp.second;
        }
        h[m] = q[m];
        return h;
    }

    /*
    vector<double> sum_expansion(const vector<double> &e, const vector<double> &f)
    {
        int m = e.size();
        int n = f.size();
        vector<double> h(n + m);
        vector<double> tmp(m + 1);
        for (int i = 0; i < n; ++i)
            h[i] = e[i];
        for (int i = 0; i < n; ++i)
        {
            tmp = grow_expansion(h, f[i], i, i + m - 1);
            for (int j = 0; j < m + 1; ++j)
                h[i + j] = tmp[j];
        }
        return h;
    }*/
    pair<double, double> split(double a)
    {
        static size_t d = std::numeric_limits<double>::digits - std::numeric_limits<double>::digits / 2;
        double c = ((1LL << d) + 1LL) * a;
        double a_big = c - a;
        double a_hi = c - a_big;
        double a_lo = a - a_hi;
        return make_pair(a_hi, a_hi);
    }

    pair<double, double> mul(double a, double b)
    {
        std::pair<double, double> sa = split(a), sb = split(b);
        double al = sa.first, ah = sa.second;
        double bl = sb.first, bh = sb.second;
        double x = a * b;
        double err1 = x - (ah * bh);
        double err2 = err1 - (al * bh);
        double err3 = err2 - (ah * bl);
        double y = al * bl - err3; // - (err3-(al * bl))
        return std::make_pair(x, y);
    }
}

int adaptive_left_turn(const point &a, const point &b, const point &c)
{
    // (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
    std::pair<double, double> p[6] =
    {
        mul(b.x, c.y),
        mul(-a.x, c.y),
        mul(-b.x, a.y),
        mul(-b.y, c.x),
        mul(a.y, c.x),
        mul(b.y, a.x)
    };

    vector<double> res;
    for (int i = 0; i < 6; i++)
    {
        res = grow_expansion(res, p[i].first);
        res = grow_expansion(res, p[i].second);
    }
    /*
    for (vector<double>::const_iterator it = res.end(); it != res.begin(); --it)
    {
        if (*it > 0)
            return 1;
        if (*it < 0)
            return -1;
    }
    */
    for (int i = res.size() - 1; i >= 0; --i)
    {
        if (res[i] > 0)
            return 1;
        if (res[i] < 0)
            return -1;
    }
    return 0;
}
